module Main where

import HQP.QOp.Syntax
import HQP.Opt (optimizeCircuitWithExtraction, optimizeCircuit)
import HQP.PrettyPrint (visualizeOutput)

-- =============================================================================
-- HELPER FUNCTIONS
-- =============================================================================

-- | Run a test case: prints the name, the input circuit, and the optimized output.
runTest :: String -> String -> [QOp] -> IO ()
runTest name desc rotList = do
    putStrLn $ "\n==========================================================="
    putStrLn $ "TEST " ++ name ++ ": " ++ desc
    putStrLn "==========================================================="
    
    let circuit = foldr1 Compose (reverse rotList) -- Compose applies right-to-left, list is [1, 2, 3]
    
    putStrLn "--- INPUT (Logical Sequence) ---"
    visualizeOutput circuit
    
    putStrLn "\n--- OPTIMIZED (Physical Sequence) ---"
    -- Note: optimizeCircuitWithExtraction expects the full circuit QOp
    visualizeOutput (optimizeCircuitWithExtraction rotList)
    putStrLn ""

    putStrLn "\n--- NAIVE DECOMPOSE AND PUSH (Physical Sequence) ---"
    -- Note: optimizeCircuitWithExtraction expects the full circuit QOp
    visualizeOutput (optimizeCircuit circuit)
    putStrLn ""

-- =============================================================================
-- SCENARIOS
-- =============================================================================

main :: IO ()
main = do
    putStrLn "Running Clifford Extraction & Smart Tree Synthesis Tests..."

    -- -------------------------------------------------------------------------
    -- 1. REGRESSION SCENARIO (The "Correctness" Proof)
    -- This matches exactly the case we debugged where Y becomes X via CNOTs.
    -- -------------------------------------------------------------------------
    -- P1: Z Z Y X X Z Y (7 Qubits)
    let p1 = R (Tensor Z (Tensor Z (Tensor Y (Tensor X (Tensor X (Tensor Z Y)))))) 1.0
    -- P2: X Y Z I X Z Y
    let p2 = R (Tensor X (Tensor Y (Tensor Z (Tensor I (Tensor X (Tensor Z Y)))))) 1.0
    -- P3: X Y I Z Y Z X
    let p3 = R (Tensor X (Tensor Y (Tensor I (Tensor Z (Tensor Y (Tensor Z X)))))) 1.0
    
    runTest "1" "Regression Scenario (Complex Propagation)" [p1, p2, p3]

    -- -------------------------------------------------------------------------
    -- 2. Y-PROPAGATION CHECK (Critical for Basis Logic)
    -- R(Y) on Q0. Uncompute is Rx.
    -- Next is R(Z) on Q0.
    -- Rx * Z * Rx† = Y. 
    -- We expect the second rotation to become R(Y) (or simplified) in the Z-frame.
    -- -------------------------------------------------------------------------
    let y1 = R Y 1.0
    let z1 = R Z 1.0
    runTest "2" "Y-Propagation (Y -> Z should become Y-Frame)" [y1, z1]

    -- -------------------------------------------------------------------------
    -- 3. COMMON SUBSTRUCTURE (Optimization Check)
    -- Two identical rotations. The second one should have a tiny CNOT tree.
    -- -------------------------------------------------------------------------
    -- R(Z Z Z Z)
    let rZ4 = R (Tensor Z (Tensor Z (Tensor Z Z))) 1.0
    runTest "3" "Common Substructure (Identical Rotations)" [rZ4, rZ4]

    -- -------------------------------------------------------------------------
    -- 4. ALTERNATING BASES (Stress Test)
    -- Forces full uncompute/recompute cycles.
    -- X(all) -> Z(all) -> X(all)
    -- -------------------------------------------------------------------------
    let rX4 = R (Tensor X (Tensor X (Tensor X X))) 1.0
    let rZ4_alt = R (Tensor Z (Tensor Z (Tensor Z Z))) 1.0
    runTest "4" "Alternating Bases (X -> Z -> X)" [rX4, rZ4_alt, rX4]

    -- -------------------------------------------------------------------------
    -- 5. DISJOINT SETS (Parallelism)
    -- R(Z Z I I) followed by R(I I X X).
    -- They should not interfere, and CNOT trees should be independent.
    -- -------------------------------------------------------------------------
    let rHead = R (Tensor Z (Tensor Z (Tensor I I))) 1.0
    let rTail = R (Tensor I (Tensor I (Tensor X X))) 1.0
    runTest "5" "Disjoint Sets (Parallel Operations)" [rHead, rTail]