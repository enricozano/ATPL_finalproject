module Main where

import HQP.QOp.Syntax
import HQP.PrettyPrint
import HQP.QOp.HelperFunctions
import HQP.Opt


-- Helper per stampare in modo pulito
runTest :: String -> String -> QOp -> IO ()
runTest name desc input = do
    putStrLn $ "\n-----------------------------------------------------------"
    putStrLn $ "TEST " ++ name ++ ": " ++ desc
    putStrLn "-----------------------------------------------------------"
    putStrLn "INPUT CIRCUIT:"
    visualizeOutput input
    putStrLn "\nOPTIMIZED OUTPUT (Decomposed & Pushed):"
    visualizeOutput (optimizeCircuit input)
    putStrLn ""

main :: IO ()
main = do
    putStrLn "========================================="
    putStrLn "   FULL OPTIMIZATION SUITE (5 CASES)     "
    putStrLn "========================================="

    -- ---------------------------------------------------------
    -- TEST 1: Basis Change Propagation (Cross-Basis)
    -- ---------------------------------------------------------
    let t1 = Compose (R Z 0.5) (R X 0.5) -- Nota: Compose A B applica B poi A
    runTest "1" "Basis Change (X then Z)" t1

    -- ---------------------------------------------------------
    -- TEST 2: Clifford Cancellation (The 'Reset')
    -- ---------------------------------------------------------
    let t2 = Compose (R X 0.5) (R X 0.5)
    runTest "2" "Clifford Cancellation (X then X)" t2

    -- ---------------------------------------------------------
    -- TEST 3: Entanglement Growth (Operator Expansion)
    -- ---------------------------------------------------------
    let rZZ = R (Tensor Z Z) 0.5
    let rXI = R (Tensor X I) 0.5
    let t3  = Compose rXI rZZ
    runTest "3" "Entanglement Growth (ZZ then XI -> XX)" t3

    -- ---------------------------------------------------------
    -- TEST 4: Entanglement Shrinkage (Operator Simplification)
    -- ---------------------------------------------------------
    let rXX = R (Tensor X X) 0.5
    let t4  = Compose rXX rZZ
    runTest "4" "Entanglement Shrinkage (ZZ then XX -> XI)" t4

    -- ---------------------------------------------------------
    -- TEST 5: The "Native" Passthrough
    -- ---------------------------------------------------------
    let t5 = Compose (Tensor (R Z 0.3) I) (R (Tensor Z Z) 0.2)
    runTest "5" "Native Z Passthrough (No Basis Change)" t5