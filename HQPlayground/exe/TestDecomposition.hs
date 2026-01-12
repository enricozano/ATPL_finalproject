module Main where

import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions
import HQP.Opt.CliffordExtraction
import HQP.Opt.GateDecomposition
import HQP.PrettyPrint

-- Helper to run push tests
runPushTest :: String -> QOp -> IO ()
runPushTest name input = do
    putStrLn $ "\n-----------------------------------------------------------"
    putStrLn $ "PUSH TEST " ++ name
    putStrLn "-----------------------------------------------------------"
    putStrLn "INPUT CIRCUIT (Clifford then Rotation):"
    visualizeOutput input
    putStrLn "\nRESULT AFTER pushCliffords (Rotation then Clifford):"
    visualizeOutput (pushCliffords input)
    putStrLn ""

main :: IO ()
main = do
    let n = 4
    
    -- 1. Basis Change Test: H gates followed by Z-Rotation
    -- H . Z . H = X. Thus, R(ZZZZ) . (H⊗H⊗H⊗H) should become (H⊗H⊗H⊗H) . R(XXXX)
    let h4 = Tensor H (Tensor H (Tensor H H))
    let rotZZZZ = R (Tensor Z (Tensor Z (Tensor Z Z))) 0.5
    runPushTest "1: Hadamard Basis Change (H -> X)" (Compose rotZZZZ h4)

    -- 2. CNOT Tree Test: CNOT Ladder followed by a single qubit rotation
    -- Moving a Z-rotation on the control qubit through a CNOT doesn't change it,
    -- but moving a Z-rotation on the target qubit creates a ZZ interaction.
    let (cnotLadder, _) = buildNaiveLadderPair [0, 1, 2, 3] n
    let rotTarget = R (Tensor I (Tensor I (Tensor I Z))) 0.8 -- Rotation on qubit 3
    runPushTest "2: CNOT Ladder Push (Z on target -> ZZZZ)" (Compose rotTarget cnotLadder)
    
    -- 4. Complex Mixed Block: A CNOT tree and Hadamards
    -- This tests the recursive nature of the extraction.
    let complexCliff = Compose cnotLadder h4
    runPushTest "4: Mixed CNOT + Hadamard Block" (Compose rotZZZZ complexCliff)

    putStrLn "Tests completed."