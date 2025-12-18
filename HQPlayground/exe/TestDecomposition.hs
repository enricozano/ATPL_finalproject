module Main where

import HQP.QOp.Syntax
import HQP.Opt.GateDecomposition
import HQP.PrettyPrint
import HQP.PrettyPrint (visualizeOutput)

main :: IO ()
main = do
    {-putStrLn "\n--- TEST 1: The Original 'Bug' (Identity) ---"
    putStrLn "Input:"
    let t1 = R (Tensor I I) 0.5
    visualizeOutput t1
    putStr "Result:"
    visualizeOutput $ expandAllPauliGadgets t1

    putStrLn "\n--- TEST 2: Gap / Long Distance (Z ⊗ I ⊗ Z) ---"
    putStrLn "Input:"
    let t2 = R (Tensor Z (Tensor I Z)) 1.0
    visualizeOutput t2
    putStr "Result: "
    visualizeOutput $ expandAllPauliGadgets t2

    putStrLn "\n--- TEST 3: Single Active Qubit in the middle (I ⊗ X ⊗ I) ---"
    putStrLn "Input:"
    let t3 = R (Tensor I (Tensor X I)) 0.5
    visualizeOutput t3
    putStr "Result:"
    visualizeOutput $ expandAllPauliGadgets t3

    putStrLn "\n--- TEST 4: Mixed Basis Change (X ⊗ Y) ---"
    putStrLn "Input"
    let t4 = R (Tensor X Y) 0.5
    visualizeOutput t4
    putStr "Result:"
    visualizeOutput $ expandAllPauliGadgets t4-}

    putStrLn "\n--- QuCLEAR img trivial decomposition ---"
    let t5 = Compose (R (Tensor Y (Tensor Y (Tensor X X))) 0.5) (R (Tensor Z (Tensor Z (Tensor Z Z))) 0.5) 
    putStrLn "Input:"
    visualizeOutput t5
    putStrLn "Result:"
    visualizeOutput $ expandAllPauliGadgets t5