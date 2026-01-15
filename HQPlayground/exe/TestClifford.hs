module Main where

-- Imported randomIO to get a seed from the operating system
import System.Random (mkStdGen, randomRs, randomIO)
import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions
import HQP.Opt (optimizeCircuitWithExtraction, optimizeCircuit)
import HQP.PrettyPrint (visualizeOutput)

-- =============================================================================
-- 1. HELPER FUNCTIONS
-- =============================================================================

-- | Recursively counts CNOT gates (C X) in a circuit.
countCNOTs :: QOp -> Int
countCNOTs op = case op of
    Compose a b   -> countCNOTs a + countCNOTs b
    Tensor a b    -> countCNOTs a + countCNOTs b
    DirectSum a b -> countCNOTs a + countCNOTs b
    Adjoint a     -> countCNOTs a
    C inner       -> countControlledX inner
    _             -> 0

countControlledX :: QOp -> Int
countControlledX op = case op of
    X           -> 1
    Tensor a b  -> countControlledX a + countControlledX b
    Compose a b -> countControlledX a + countControlledX b
    Adjoint a   -> countControlledX a
    _           -> 0

-- | Helper to create Rotations from a string (e.g. "ZZII...")
mkRot :: String -> Double -> QOp
mkRot s theta = R (foldr1 Tensor (map charToPauli s)) theta
  where
    charToPauli 'I' = I
    charToPauli 'X' = X
    charToPauli 'Y' = Y
    charToPauli 'Z' = Z
    charToPauli c   = error $ "Unknown Pauli: " ++ [c]

-- | Deterministic Random Rotation Generator using System.Random
-- Creates an unstructured Pauli string of length 'nQubits'.
-- Uses mkStdGen with a fixed seed to ensure reproducibility within a single run.
mkRandomRot :: Int -> Int -> QOp
mkRandomRot seed nQubits = 
    let 
        gen = mkStdGen seed
        -- Generate nQubits integers between 0 and 3
        randomInts = take nQubits $ randomRs (0, 3) gen
        
        toPauli :: Int -> QOp
        toPauli 0 = I
        toPauli 1 = X
        toPauli 2 = Y
        toPauli 3 = Z
        toPauli _ = I
        
        pauliList = map toPauli randomInts
    in
        R (foldr1 Tensor pauliList) 0.5

-- =============================================================================
-- 2. TEST RUNNERS
-- =============================================================================

-- | Detailed test: Prints full circuits (For the regression case)
runDetailedTest :: String -> String -> [QOp] -> IO ()
runDetailedTest name desc rotList = do
    putStrLn $ "\n==========================================================="
    putStrLn $ "DETAILED TEST " ++ name ++ ": " ++ desc
    putStrLn "==========================================================="
    
    let circuit = foldr1 Compose (reverse rotList)
    
    putStrLn "--- INPUT (Logical Sequence) ---"
    visualizeOutput circuit
    
    putStrLn "\n--- SMART OPTIMIZATION (Extraction) ---"
    let smartCirc = optimizeCircuitWithExtraction rotList
    visualizeOutput smartCirc
    
    putStrLn "\n--- NAIVE OPTIMIZATION (Decompose & Push) ---"
    let naiveCirc = optimizeCircuit circuit
    visualizeOutput naiveCirc
    putStrLn ""

-- | Comparison test: Prints only CNOT counts (For the large benchmark cases)
runComparisonTest :: String -> String -> [QOp] -> IO ()
runComparisonTest name desc rotList = do
    putStrLn $ "-----------------------------------------------------------"
    putStrLn $ "BENCHMARK " ++ name ++ ": " ++ desc
    
    let logicalCirc = foldr1 Compose (reverse rotList)

    -- 1. Smart Extraction
    putStrLn "  > Running Smart Extraction..."
    let smartCirc = optimizeCircuitWithExtraction rotList
    let smartCNOTs = countCNOTs smartCirc
    putStrLn $ "    Done. CNOTs: " ++ show smartCNOTs

    -- 2. Naive Decompose & Push
    putStrLn "  > Running Naive Optimization..."
    let naiveCirc = optimizeCircuit logicalCirc
    let naiveCNOTs = countCNOTs naiveCirc
    putStrLn $ "    Done. CNOTs: " ++ show naiveCNOTs
    
    let saving = if naiveCNOTs > 0 
                 then (fromIntegral (naiveCNOTs - smartCNOTs) / fromIntegral naiveCNOTs) * 100 
                 else 0.0 :: Double
                 
    putStrLn $ "  > Reduction:   " ++ take 5 (show saving) ++ "%"
    putStrLn "-----------------------------------------------------------\n"

-- =============================================================================
-- 3. MAIN SUITE
-- =============================================================================

main :: IO ()
main = do
    putStrLn "\n=== CLIFFORD OPTIMIZATION SUITE ===\n"

    -- -------------------------------------------------------------------------
    -- TEST 1: The Regression Scenario (Detailed)
    -- -------------------------------------------------------------------------
    -- P1: Z Z Y X X Z Y 
    let p1 = mkRot "ZZYXXZY" 1.0
    -- P2: X Y Z I X Z Y
    let p2 = mkRot "XYZIXZY" 1.0
    -- P3: X Y I Z Y Z X
    let p3 = mkRot "XYIZYZX" 1.0
    
    runDetailedTest "1" "Regression (Propagation Check)" [p1, p2, p3]

    putStrLn "\n=== 10-QUBIT / DEPTH-5 BENCHMARKS ===\n"

    -- -------------------------------------------------------------------------
    -- BENCHMARK A: High Overlap (Dense)
    -- -------------------------------------------------------------------------
    let ba1 = mkRot "ZZZZZXXXXX" 0.5
    let ba2 = mkRot "ZZZZZYYYYY" 0.5 
    let ba3 = mkRot "XXXXXZZZZZ" 0.5 
    let ba4 = mkRot "YYYYYZZZZZ" 0.5 
    let ba5 = mkRot "ZZZZZXXXXX" 0.5 

    runComparisonTest "A" "High Overlap / Dense (10Q, Depth 5)" [ba1, ba2, ba3, ba4, ba5]

    -- -------------------------------------------------------------------------
    -- BENCHMARK B: Sparse / Localized
    -- -------------------------------------------------------------------------
    let bb1 = mkRot "ZZIIIIIIII" 0.5 
    let bb2 = mkRot "IIXXIIIIII" 0.5 
    let bb3 = mkRot "IIIIYYIIII" 0.5 
    let bb4 = mkRot "IIIIIIZZII" 0.5 
    let bb5 = mkRot "IIIIIIIIXX" 0.5 

    runComparisonTest "B" "Sparse / Disjoint (10Q, Depth 5)" [bb1, bb2, bb3, bb4, bb5]

    -- -------------------------------------------------------------------------
    -- BENCHMARK C: Complex Interleaved
    -- -------------------------------------------------------------------------
    let bc1 = mkRot "XZXZXZXZXZ" 0.5
    let bc2 = mkRot "YIYIYIYIYI" 0.5
    let bc3 = mkRot "ZXZXZXZXZX" 0.5
    let bc4 = mkRot "IYIYIYIYIY" 0.5
    let bc5 = mkRot "XZXZXZXZXZ" 0.5

    runComparisonTest "C" "Interleaved / Alternating (10Q, Depth 5)" [bc1, bc2, bc3, bc4, bc5]

    putStrLn "\n=== LARGE SCALE STRESS TESTS (TRUE RANDOM) ===\n"

    -- Get a random integer from the system
    masterSeed <- randomIO :: IO Int
    
    putStrLn $ ">>> Initializing Random Generator with System Seed: " ++ show masterSeed
    putStrLn ">>> Different runs will now produce different circuits."

    -- -------------------------------------------------------------------------
    -- BENCHMARK D: Unstructured / Random (20 Qubits, Depth 20)
    -- -------------------------------------------------------------------------
    let randomRotations20 = [ mkRandomRot (masterSeed + i) 20 | i <- [0..19] ]
    runComparisonTest "D" "Unstructured / Random (20Q, Depth 20)" randomRotations20

    -- -------------------------------------------------------------------------
    -- BENCHMARK E: Large Scale (100 Qubits, Depth 100)
    -- -------------------------------------------------------------------------
    putStrLn ">>> Generating large scale circuit (100 Qubits, Depth 100)..."
    -- Note: We add a large offset to the seed to ensure independence from the previous test
    let largeRotations = [ mkRandomRot (masterSeed + 1000 + i) 100 | i <- [0..99] ]
    
    runComparisonTest "E" "Large Scale (100Q, Depth 100)" largeRotations