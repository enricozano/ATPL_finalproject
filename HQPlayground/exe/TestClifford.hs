module Main where

import System.CPUTime
import Text.Printf
import System.Random (newStdGen, randomRs)

-- MODULE IMPORTS
import HQP.QOp.Syntax
import HQP.QOp.Symplectic 
import HQP.QOp.HelperFunctions
import HQP.Opt.GateDecomposition (optimizeCircuit, toZBasis, expandPauliRot, decomposePauliRotTuple)
import HQP.Opt.SmartExtraction (optimizeCircuitWithExtraction, countCNOTs)
import HQP.PrettyPrint.PrettyOp (visualizeOutput, showOp)

-- =========================================================================
--  1. HELPER UTILITIES
-- =========================================================================

-- Helper to create rotations from strings as requested (e.g. "ZZYXXZY")
parsePauliString :: String -> QOp
parsePauliString s = foldr1 Tensor (map charToOp s)
  where
    charToOp 'I' = I
    charToOp 'X' = X
    charToOp 'Y' = Y
    charToOp 'Z' = Z
    charToOp _   = I -- Fallback

mkRot :: String -> Double -> QOp
mkRot s theta = R (parsePauliString s) theta

-- Random generator (kept for benchmarks)
randomPauli :: Int -> [Int] -> QOp
randomPauli size ints = 
    let mapIntToPauli 0 = I
        mapIntToPauli 1 = X
        mapIntToPauli 2 = Y
        mapIntToPauli 3 = Z
        mapIntToPauli _ = I
        
        paulis = map mapIntToPauli (take size ints)
        
        tensorAll [] = One
        tensorAll [x] = x
        tensorAll (x:xs) = Tensor x (tensorAll xs)
    in tensorAll paulis

generateRandomCircuit :: Int -> Int -> IO [QOp]
generateRandomCircuit nQubits depth = do
    g <- newStdGen
    let randomInts = randomRs (0, 3) g :: [Int]
    let randomDoubles = randomRs (0.0, 2 * pi) g :: [Double]
    
    let makeLayer i = 
            let pString = randomPauli nQubits (drop (i*nQubits) randomInts)
                angle   = randomDoubles !! i
            in R pString angle

    return $ map makeLayer [0..depth-1]


-- =========================================================================
--  2. UNIT TESTS (Core Functionality)
-- =========================================================================

testCommutationRules :: IO ()
testCommutationRules = do
    putStrLn "\n=== TEST 1: Clifford Commutation Rules (Multi-Qubit) ==="
    
    putStrLn "\n1. Tensor Conjugation (H ⊗ H ⊗ H) on R(X ⊗ Z ⊗ X):"
    let rotA   = mkRot "XZX" 1.57
    let cliffA = Tensor H (Tensor H H)
    let resA   = applyCliffordToRotation cliffA rotA
    
    printf "   Input:  %s\n   Expected: R(Z ⊗ X ⊗ Z)\n   Actual:   %s\n" (showOp rotA) (showOp resA)

    putStrLn "\n2. Conjugation CNOT(0,3) on R(I ⊗ I ⊗ I ⊗ Z):"
    let rotB   = mkRot "IIIZ" 3.14
    let cliffB = genericCNOT 0 3 4 -- Use genericCNOT to create the correct distant CNOT
    let resB   = applyCliffordToRotation cliffB rotB
    
    printf "   Input:  %s\n   Expected: R(Z ⊗ I ⊗ I ⊗ Z)\n   Actual:   %s\n" (showOp rotB) (showOp resB)

testZBasisMapping :: IO ()
testZBasisMapping = do
    putStrLn "\n=== TEST 2: Z-Basis Mapping (Pauli Gadgets - 6 Qubit) ==="
    
    let str = "XYZIXZ"
    let pauliString = parsePauliString str
    
    putStrLn $ "Original Pauli Operator: " ++ str
    
    -- Calculate basis change
    let basisChange = toZBasis pauliString
    putStrLn $ "Basis change Clifford (toZBasis): " ++ showOp basisChange
    
    -- Verify: Apply basis change to rotation. Must become all Z (or I where there was I).
    let originalRot = R pauliString 1.0
    let transformedRot = applyCliffordToRotation basisChange originalRot
    
    putStrLn $ "Result after conjugation (Should be only Z and I): " ++ showOp transformedRot

testNaiveDecomposition :: IO ()
testNaiveDecomposition = do
    putStrLn "\n=== TEST 3: Naive Decomposition (Test expandPauliRot) ==="
    putStrLn "Goal: Verify complete decomposition of a 7-qubit rotation."
    
    -- Rotation on 7 qubits with mixed Paulis
    let str = "XIZYIXZ"
    let rot = mkRot str 0.5
    
    putStrLn $ "\nInput Rotation: " ++ showOp rot
    
    -- 1. Get components (Post, Core, Pre)
    let (post, core, pre) = decomposePauliRotTuple rot
    
    -- 2. Use expandPauliRot as requested to reassemble everything
    let fullGadget = expandPauliRot (post, core, pre)
    
    putStrLn "\n--- Expanded Circuit (Visualization) ---"
    visualizeOutput fullGadget


-- =========================================================================
--  3. ALGORITHM TESTS (Optimization Strategies)
-- =========================================================================

testOptimizationStrategies :: IO ()
testOptimizationStrategies = do
    putStrLn "\n=== TEST 4: QuCLEAR fig 7 case ==="
    
    let p1 = mkRot "ZZYXXZY" 1.0
    let p2 = mkRot "XYZIXZY" 1.0
    let p3 = mkRot "XYIZYZX" 1.0
    
    let circuitList = [p1, p2, p3]
    let fullCircuit = foldl1 Compose circuitList 
    
    putStrLn "\n--- Original Circuit ---"
    visualizeOutput fullCircuit
    
    putStrLn "\n--- 1. Naive Optimization (Decompose & Push) ---"
    let naiveResult = optimizeCircuit fullCircuit
    visualizeOutput naiveResult
    let naiveCNOTs = countCNOTs naiveResult
    printf "CNOT count Naive: %d\n" naiveCNOTs
    
    putStrLn "\n--- 2.  Smart Optimization (QuCLEAR) ---"
    let smartResult = optimizeCircuitWithExtraction circuitList
    visualizeOutput smartResult
    let smartCNOTs = countCNOTs smartResult
    printf "CNOT count Smart: %d\n" smartCNOTs
    
    printf "CNOT reduction: %.1f%%\n" ((fromIntegral (naiveCNOTs - smartCNOTs) / fromIntegral naiveCNOTs * 100) :: Double)


-- =========================================================================
--  4. BENCHMARKS
-- =========================================================================

timeAndCount :: IO QOp -> IO (Double, Int)
timeAndCount action = do
    start <- getCPUTime
    result <- action
    let count = countCNOTs result
    end <- count `seq` getCPUTime
    let diff = (fromIntegral (end - start)) / 1e12
    return (diff, count)

runBenchmarkCase :: Int -> Int -> IO ()
runBenchmarkCase n d = do
    printf "\n--- Benchmarking: %d Qubits, %d Depth ---\n" n d
    circuitOps <- generateRandomCircuit n d
    let rawCircuit = foldl Compose One circuitOps 
    
    (timeNaive, cnotNaive) <- timeAndCount (return $ optimizeCircuit rawCircuit)
    (timeSmart, cnotSmart) <- timeAndCount (return $ optimizeCircuitWithExtraction circuitOps)
    
    printf "Naive: %4d CNOTs | Time: %.4fs\n" cnotNaive timeNaive
    printf "Smart: %4d CNOTs | Time: %.4fs\n" cnotSmart timeSmart
    
    let savings = cnotNaive - cnotSmart
    let percent = (fromIntegral savings / fromIntegral cnotNaive) * 100 :: Double
    printf ">> IMPROVEMENT: Saved %d CNOTs (%.1f%% reduction)\n" savings percent

benchmarkScalability :: IO ()
benchmarkScalability = do
    putStrLn "\n=========================================="
    putStrLn "    SCALABILITY BENCHMARKS"
    putStrLn "=========================================="
    runBenchmarkCase 10 10
    runBenchmarkCase 20 20
    runBenchmarkCase 50 50
    --runBenchmarkCase 100 100

-- =========================================================================
--  MAIN
-- =========================================================================

main :: IO ()
main = do
    testCommutationRules
    testZBasisMapping
    testNaiveDecomposition
    testOptimizationStrategies
    benchmarkScalability
    putStrLn "\nAll tests completed."