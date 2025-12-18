module Main where

import HQP.QOp.Syntax
import HQP.PrettyPrint
import HQP.Opt

-- Helpers per i CNOT
cnot01 = Tensor (C X) (Tensor I I)
cnot12 = Tensor I (Tensor (C X) I)
cnot23 = Tensor I (Tensor I (C X))

main :: IO ()
main = do
    putStrLn "Starting Visualizer Test Suite..."

    -- TEST 1
    --putStrLn "\n----HADAMRD TEST----\n"
    --visualizeOutput (Compose (R Z 0.5) H)

    -- TEST 2: THE MONSTER
    let layer_H = Tensor H (Tensor H (Tensor H H))
    let cliff_A = Compose (Compose cnot01 cnot23) layer_H
    let rot_1   = R (Tensor Z (Tensor Z (Tensor I I))) 0.2
    let cliff_B = Compose cnot12 (Tensor I (Tensor SX (Tensor I I)))
    let rot_2   = R (Tensor I (Tensor X (Tensor Z Y))) 0.5
    let cliff_C = Tensor H (Tensor I (Tensor I X))
    let rot_3   = R (Tensor Z (Tensor I (Tensor I I))) 0.8
    
    let monster = Compose rot_3 (Compose cliff_C (Compose rot_2 (Compose cliff_B (Compose rot_1 cliff_A))))

    putStrLn "\n----MONSTER INPUT----\n"
    visualizeOutput  monster
    putStrLn "\n----MONSTER CLIFFORD MOVED----\n"
    visualizeOutput (pushCliffords monster)