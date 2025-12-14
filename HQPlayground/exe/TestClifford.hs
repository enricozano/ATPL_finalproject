module Main where

import HQP.QOp.Syntax
import HQP.QOp.CliffordExtraction
import Text.Printf (printf)

-- =================================================================
-- 1. PRETTY PRINTER & UTILITIES
-- =================================================================

-- | Formatta una Pauli String (es. "X I Z Y")
ppPauli :: QOp -> String
ppPauli op = case op of
    I -> "I"; X -> "X"; Y -> "Y"; Z -> "Z"
    Tensor a b -> ppPauli a ++ ppPauli b
    One -> ""; _ -> "?"

-- | Formatta un Clifford (es. "H ⊗ CNOT")
ppClifford :: QOp -> String
ppClifford op = case op of
    I -> "I"; X -> "X"; Y -> "Y"; Z -> "Z"; H -> "H"; SX -> "SX"
    C X -> "CNOT"
    Tensor a b -> ppClifford a ++ " ⊗ " ++ ppClifford b
    Compose a b -> ppClifford a ++ " . " ++ ppClifford b
    Permute ks -> "Perm " ++ show ks
    Adjoint a -> "(" ++ ppClifford a ++ ")†"
    One -> "1"
    _ -> show op

-- | Appiattisce l'albero Compose in una lista
flattenOps :: QOp -> [QOp]
flattenOps (Compose outer inner) = flattenOps outer ++ flattenOps inner
flattenOps op 
    | isIdentity op = [] 
    | otherwise = [op]

-- | Verifica se un operatore è pura identità
isIdentity :: QOp -> Bool
isIdentity I = True; isIdentity One = True
isIdentity (Tensor a b) = isIdentity a && isIdentity b
isIdentity _ = False

-- | Stampa una riga formattata dell'operatore
printOpItem :: (Int, QOp) -> IO ()
printOpItem (i, o) = case o of
    R pauli theta -> printf "   %2d. R( %-6s )  θ = %6.3f\n" i (ppPauli pauli) theta
    _ -> printf "   %2d. %s\n" i (ppClifford o)

-- =================================================================
-- 2. VISUALIZER ENGINE
-- =================================================================

visualizeOutput :: String -> QOp -> IO ()
visualizeOutput title inputOp = do
    putStrLn $ "\n" ++ replicate 75 '='
    putStrLn $ " TEST: " ++ title
    putStrLn $ replicate 75 '='
    
    putStrLn "\n  [INPUT] CIRCUITO ORIGINALE (Sequenza Temporale):"
    let initialList = reverse (flattenOps inputOp) 
    
    if null initialList 
        then putStrLn "      (Vuoto)"
        else mapM_ printOpItem (zip ([1..] :: [Int]) initialList)

    -- ESECUZIONE ALGORITMO
    let processedOp = pushCliffords inputOp
    
    putStrLn "\n  [OUTPUT] CIRCUITO PROCESSATO (Rotazioni -> Cliffords):"
    let finalList = reverse (flattenOps processedOp)
    
    if null finalList 
        then putStrLn "      (Identità)"
        else mapM_ printOpItem (zip ([1..] :: [Int]) finalList)

-- =================================================================
-- 3. HELPER PER CIRCUITI COMPLESSI
-- =================================================================

-- Definiamo i CNOT tensoriali manualmente per chiarezza
cnot01, cnot12, cnot23 :: QOp
cnot01 = Tensor (C X) (Tensor I I)
cnot12 = Tensor I (Tensor (C X) I)
cnot23 = Tensor I (Tensor I (C X))

-- =================================================================
-- 4. MAIN TEST SUITE
-- =================================================================

main :: IO ()
main = do
    putStrLn "Avvio Suite di Test (Logica: Push Cliffords to Future)..."

    -- ---------------------------------------------------------
    -- TEST 1: HADAMARD PUSH
    -- ---------------------------------------------------------
    let t1 = Compose (R Z 0.5) H
    visualizeOutput "Hadamard Push (H -> Rz  =>  Rx -> H)" t1

    -- ---------------------------------------------------------
    -- TEST 2: CNOT PUSH BACK (Target)
    -- ---------------------------------------------------------
    let t2 = Compose (R (Tensor I Z) 1.0) (C X)
    visualizeOutput "CNOT Z-Kickback (CNOT -> Rz_target  =>  Rzz -> CNOT)" t2

    -- ---------------------------------------------------------
    -- TEST 3: CNOT PUSH BACK (Control)
    -- ---------------------------------------------------------
    let t3 = Compose (R (Tensor X I) 0.8) (C X)
    visualizeOutput "CNOT X-Spread (CNOT -> Rx_control  =>  Rxx -> CNOT)" t3

    -- ---------------------------------------------------------
    -- TEST 4: SANDWICH
    -- ---------------------------------------------------------
    let t4 = Compose H (Compose (R X 1.57) H)
    visualizeOutput "Sandwich (H -> Rx -> H  =>  Rz)" t4

    -- ---------------------------------------------------------
    -- TEST 5: THE MONSTER (4 Qubits, 3 Layers)
    -- ---------------------------------------------------------
    
    -- LAYER 1 (Start Time): Entanglement H + CNOT
    -- Tensore esplicito: H (x) H (x) H (x) H
    let layer_H = Tensor H (Tensor H (Tensor H H))
    let cliff_A = Compose (Compose cnot01 cnot23) layer_H
    
    -- LAYER 2: Rotazione ZZ (Z Z I I)
    -- Tensore esplicito: Z (x) Z (x) I (x) I
    let rot_1_pauli = Tensor Z (Tensor Z (Tensor I I))
    let rot_1   = R rot_1_pauli 0.2
    
    -- LAYER 3: SX e CNOT centrale (I SX I I)
    -- Tensore esplicito: I (x) SX (x) I (x) I
    let layer_SX = Tensor I (Tensor SX (Tensor I I))
    let cliff_B = Compose cnot12 layer_SX
    
    -- LAYER 4: Rotazione complessa (I X Z Y)
    -- Tensore esplicito: I (x) X (x) Z (x) Y
    let rot_2_pauli = Tensor I (Tensor X (Tensor Z Y))
    let rot_2   = R rot_2_pauli 0.5
    
    -- LAYER 5: Cambio base (H I I X)
    -- Tensore esplicito: H (x) I (x) I (x) X
    let cliff_C = Tensor H (Tensor I (Tensor I X))
    
    -- LAYER 6 (End Time): Rotazione Z finale (Z I I I)
    -- Tensore esplicito: Z (x) I (x) I (x) I
    let rot_3_pauli = Tensor Z (Tensor I (Tensor I I))
    let rot_3   = R rot_3_pauli 0.8
    
    -- Assemblaggio (Ordine Compose è Inverso temporale: Ultimo . ... . Primo)
    let monster = Compose rot_3 (Compose cliff_C (Compose rot_2 (Compose cliff_B (Compose rot_1 cliff_A))))

    visualizeOutput "THE MONSTER (3 Rot layers, 4 Qubits)" monster