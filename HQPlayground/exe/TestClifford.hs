module Main where

import HQP.QOp.Syntax
import HQP.QOp.CliffordExtraction
import Text.Printf (printf)

-- =================================================================
-- 1. PRETTY PRINTER & UTILITIES
-- =================================================================

-- | Formats a Pauli String (e.g., "X I Z Y")
--   Adds a space between tensor components for better readability.
ppPauli :: QOp -> String
ppPauli op = case op of
    I -> "I"
    X -> "X"
    Y -> "Y"
    Z -> "Z"
    Tensor a b -> ppPauli a ++ " " ++ ppPauli b
    One -> ""
    _ -> "?"

-- | Formats a Clifford operator (e.g., "H ⊗ CNOT")
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

-- | Flattens the Compose tree into a list
flattenOps :: QOp -> [QOp]
flattenOps (Compose outer inner) = flattenOps outer ++ flattenOps inner
flattenOps op 
    | isIdentity op = [] 
    | otherwise = [op]

-- | Checks if an operator is pure identity
isIdentity :: QOp -> Bool
isIdentity I = True; isIdentity One = True
isIdentity (Tensor a b) = isIdentity a && isIdentity b
isIdentity _ = False

-- | Prints a formatted line for the operator item
printOpItem :: (Int, QOp) -> IO ()
printOpItem (i, o) = case o of
    -- Adjusted padding (%-10s) to handle longer Pauli strings like "Z I I I"
    R pauli theta -> printf "   %2d. R( %-10s )  θ = %6.3f\n" i (ppPauli pauli) theta
    _ -> printf "   %2d. %s\n" i (ppClifford o)

-- =================================================================
-- 2. VISUALIZER ENGINE
-- =================================================================

visualizeOutput :: String -> QOp -> IO ()
visualizeOutput title inputOp = do
    putStrLn $ "\n" ++ replicate 75 '='
    putStrLn $ " TEST: " ++ title
    putStrLn $ replicate 75 '='
    
    putStrLn "\n  [INPUT] ORIGINAL CIRCUIT (Time Sequence):"
    let initialList = reverse (flattenOps inputOp) 
    
    if null initialList 
        then putStrLn "      (Empty)"
        else mapM_ printOpItem (zip ([1..] :: [Int]) initialList)

    -- ALGORITHM EXECUTION
    let processedOp = pushCliffords inputOp
    
    putStrLn "\n  [OUTPUT] PROCESSED CIRCUIT (Rotations -> Cliffords):"
    let finalList = reverse (flattenOps processedOp)
    
    if null finalList 
        then putStrLn "      (Identity)"
        else mapM_ printOpItem (zip ([1..] :: [Int]) finalList)

-- =================================================================
-- 3. HELPERS FOR COMPLEX CIRCUITS
-- =================================================================

-- Manually defining tensor CNOTs for clarity
cnot01, cnot12, cnot23 :: QOp
cnot01 = Tensor (C X) (Tensor I I)
cnot12 = Tensor I (Tensor (C X) I)
cnot23 = Tensor I (Tensor I (C X))

-- =================================================================
-- 4. MAIN TEST SUITE
-- =================================================================

main :: IO ()
main = do
    putStrLn "Starting Test Suite (Logic: Push Cliffords to Future)..."

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
    -- Explicit Tensor: H (x) H (x) H (x) H
    let layer_H = Tensor H (Tensor H (Tensor H H))
    let cliff_A = Compose (Compose cnot01 cnot23) layer_H
    
    -- LAYER 2: ZZ Rotation (Z Z I I)
    -- Explicit Tensor: Z (x) Z (x) I (x) I
    let rot_1_pauli = Tensor Z (Tensor Z (Tensor I I))
    let rot_1   = R rot_1_pauli 0.2
    
    -- LAYER 3: SX and central CNOT (I SX I I)
    -- Explicit Tensor: I (x) SX (x) I (x) I
    let layer_SX = Tensor I (Tensor SX (Tensor I I))
    let cliff_B = Compose cnot12 layer_SX
    
    -- LAYER 4: Complex Rotation (I X Z Y)
    -- Explicit Tensor: I (x) X (x) Z (x) Y
    let rot_2_pauli = Tensor I (Tensor X (Tensor Z Y))
    let rot_2   = R rot_2_pauli 0.5
    
    -- LAYER 5: Basis Change (H I I X)
    -- Explicit Tensor: H (x) I (x) I (x) X
    let cliff_C = Tensor H (Tensor I (Tensor I X))
    
    -- LAYER 6 (End Time): Final Z Rotation (Z I I I)
    -- Explicit Tensor: Z (x) I (x) I (x) I
    let rot_3_pauli = Tensor Z (Tensor I (Tensor I I))
    let rot_3   = R rot_3_pauli 0.8
    
    -- Assembly (Compose order is Reverse Time: Last . ... . First)
    let monster = Compose rot_3 (Compose cliff_C (Compose rot_2 (Compose cliff_B (Compose rot_1 cliff_A))))

    visualizeOutput "THE MONSTER (3 Rot layers, 4 Qubits)" monster