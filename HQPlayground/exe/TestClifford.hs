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

    putStrLn "========================================="
    putStrLn "        TEST SUITE: Y ROTATIONS          "
    putStrLn "========================================="

    -- ---------------------------------------------------------
    -- TEST Y1: Simple Y -> Z
    -- R(Y) decompone in (H Sdag) . Rz . (S H).
    -- Il frame (S H) viene spinto verso R(Z).
    -- (S H) Z (S H)^dag = S H Z H Sdag = S X Sdag = Y.
    -- Quindi ci aspettiamo che la successiva R(Z) diventi R(Y) (o simile).
    -- ---------------------------------------------------------
    let y1 = Compose (R Z 0.5) (R Y 0.5) -- Ordine: Y poi Z
    runTest "Y1" "Single Qubit: R(Y) then R(Z)" y1

    -- ---------------------------------------------------------
    -- TEST Y2: Y -> Y
    -- R(Y) genera frame. La successiva R(Y) viene investita.
    -- Se R(Y) trasforma la base in Y, la successiva R(Y) dovrebbe
    -- apparire come una rotazione nativa R(Z) (o Rz con fasi).
    -- ---------------------------------------------------------
    let y2 = Compose (R Y 0.5) (R Y 0.5)
    runTest "Y2" "Single Qubit: R(Y) then R(Y)" y2

    -- ---------------------------------------------------------
    -- TEST Y3: Y -> X
    -- Verifica l'interazione tra le basi coniugate.
    -- ---------------------------------------------------------
    let y3 = Compose (R X 0.5) (R Y 0.5)
    runTest "Y3" "Single Qubit: R(Y) then R(X)" y3

    -- ---------------------------------------------------------
    -- TEST Y4: Global YY (Entanglement in Y basis)
    -- R(YY) richiede cambio di base su entrambi i qubit.
    -- Ci aspettiamo CNOT, ma con gate S e H sparsi.
    -- ---------------------------------------------------------
    let y4 = R (Tensor Y Y) 0.8
    runTest "Y4" "Multi Qubit: R(Y ⊗ Y)" y4

    -- ---------------------------------------------------------
    -- TEST Y5: Interaction YY then ZI
    -- Vediamo se il frame complesso generato da YY si propaga correttamente
    -- trasformando una rotazione Z locale.
    -- ---------------------------------------------------------
    let y5 = Compose (R (Tensor Z I) 0.3) (R (Tensor Y Y) 0.8)
    runTest "Y5" "Multi Qubit: R(Y ⊗ Y) then R(Z ⊗ I)" y5

    --TODO: CHECK CAREFULLY THE OUTPUTS OF THESE TESTS! (in particular the signs of the rotations in the Y tests)