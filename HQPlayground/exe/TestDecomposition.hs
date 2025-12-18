module Main where

import HQP.QOp.Syntax
import HQP.Opt.GateDecomposition

main :: IO ()
main = do
    putStrLn "\n--- TEST 1: The Original 'Bug' (Identity) ---"
    putStrLn "Input: R (I ⊗ I) 0.5"
    putStrLn "Expected: I ⊗ I (No CNOTs, No Rotation)"
    let t1 = R (Tensor I I) 0.5
    putStr "Result: "
    pp $ expandAllPauliGadgets t1

    putStrLn "\n--- TEST 2: Gap / Long Distance (Z ⊗ I ⊗ Z) ---"
    putStrLn "Input: R (Z ⊗ I ⊗ Z) 1.0"
    putStrLn "Expected: CNOT between 0 and 2 (skipping 1), Rotation on 2"
    -- Note: Tensor is right-associative here: Z `Tensor` (I `Tensor` Z)
    let t2 = R (Tensor Z (Tensor I Z)) 1.0
    putStr "Result: "
    pp $ expandAllPauliGadgets t2

    putStrLn "\n--- TEST 3: Single Active Qubit in the middle (I ⊗ X ⊗ I) ---"
    putStrLn "Input: R (I ⊗ X ⊗ I) 0.5"
    putStrLn "Expected: No CNOTs. Only basis change (H) and rotation on the central qubit."
    let t3 = R (Tensor I (Tensor X I)) 0.5
    putStr "Result: "
    pp $ expandAllPauliGadgets t3

    putStrLn "\n--- TEST 4: Mixed Basis Change (X ⊗ Y) ---"
    putStrLn "Input: R (X ⊗ Y) 0.5"
    putStrLn "Expected: H on Q0, (S_dag H) on Q1, then CNOT -> Rz -> CNOT, then inverse."
    let t4 = R (Tensor X Y) 0.5
    putStr "Result: "
    pp $ expandAllPauliGadgets t4