module Main where

import HQP.QOp.Syntax
import HQP.PrettyPrint
import HQP.QOp.HelperFunctions
import HQP.Opt

main :: IO ()
main = do
    putStrLn "========================================="
    putStrLn "      HQP OPTIMIZATION TEST SUITE        "
    putStrLn "========================================="

    ---------------------------------------------------------
    -- PART 1: Clifford Extraction Tests (from TestClifford.hs)
    ---------------------------------------------------------
    putStrLn "\n\n--- PART 1: CLIFFORD EXTRACTION & MOVING ---"
    
    -- "THE MONSTER" TEST
    let n = 4 

    let c01 = genericCNOT 0 1 n
    let c12 = genericCNOT 1 2 n
    let c23 = genericCNOT 2 3 n

    let layer_H = Tensor H (Tensor H (Tensor H H))
    
    let cliff_A = Compose (Compose c01 c23) layer_H
    let rot_1   = R (Tensor Z (Tensor Z (Tensor I I))) 0.2
    let cliff_B = Compose c12 (Tensor I (Tensor SX (Tensor I I)))
    let rot_2   = R (Tensor I (Tensor X (Tensor Z Y))) 0.5
    let cliff_C = Tensor H (Tensor I (Tensor I X))
    let rot_3   = R (Tensor Z (Tensor I (Tensor I I))) 0.8
    
    let monster = Compose rot_3 (Compose cliff_C (Compose rot_2 (Compose cliff_B (Compose rot_1 cliff_A))))

    putStrLn "\n[Test 1.1] The Monster Circuit"
    putStrLn "Original Input:"
    visualizeOutput monster
    
    putStrLn "\nAfter Pushing Cliffords to the End:"
    visualizeOutput (pushCliffords monster)


    putStrLn "\n\n--- PART 2: PAULI GADGET DECOMPOSITION ---"

    putStrLn "\n[Test 2.1] The Original 'Bug' (Identity)"
    let t1 = R (Tensor I I) 0.5
    putStrLn "Input:"
    visualizeOutput t1
    putStr "Result (Decomposed):"
    visualizeOutput $ expandAllPauliGadgets t1

    putStrLn "\n[Test 2.2] Gap / Long Distance (Z ⊗ I ⊗ Z)"
    let t2 = R (Tensor Z (Tensor I Z)) 1.0
    putStrLn "Input:"
    visualizeOutput t2
    putStr "Result (Decomposed): "
    visualizeOutput $ expandAllPauliGadgets t2

    putStrLn "\n[Test 2.3] Single Active Qubit in the middle (I ⊗ X ⊗ I)"
    let t3 = R (Tensor I (Tensor X I)) 0.5
    putStrLn "Input:"
    visualizeOutput t3
    putStr "Result (Decomposed):"
    visualizeOutput $ expandAllPauliGadgets t3

    putStrLn "\n[Test 2.4] Mixed Basis Change (X ⊗ Y)"
    let t4 = R (Tensor X Y) 0.5
    putStrLn "Input:"
    visualizeOutput t4
    putStr "Result (Decomposed):"
    visualizeOutput $ expandAllPauliGadgets t4

    putStrLn "\n[Test 2.5] QuCLEAR img trivial decomposition"
    let t5 = Compose (R (Tensor Y (Tensor Y (Tensor X X))) 0.5) (R (Tensor Z (Tensor Z (Tensor Z Z))) 0.5) 
    putStrLn "Input:"
    visualizeOutput t5
    putStrLn "Result (Decomposed):"
    visualizeOutput $ expandAllPauliGadgets t5

    putStrLn "\n\n========================================="
    putStrLn "           ALL TESTS COMPLETED           "
    putStrLn "========================================="