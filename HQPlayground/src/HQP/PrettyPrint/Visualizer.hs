module HQP.PrettyPrint.Visualizer where

import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions
import HQP.Opt.CliffordExtraction (isClifford, pushCliffords)
import Text.Printf (printf)

-- | Formats a Pauli string, removing identity noise
ppPauli :: QOp -> String
ppPauli op = case op of
    I          -> "I"
    X          -> "X"
    Y          -> "Y"
    Z          -> "Z"
    Tensor a b -> ppPauli a ++ " " ++ ppPauli b
    One        -> ""
    _          -> "?"

-- | Formats Clifford and Gate operators with recursive cleanup
ppClifford :: QOp -> String
ppClifford op = case op of
    -- Basic Gates
    I  -> "I"
    X  -> "X"
    Y  -> "Y"
    Z  -> "Z"
    H  -> "H"
    SX -> "SX"
    One -> "I" -- Represent identity as 'I' for visual consistency
    
    -- Handle Controlled Gates (CNOT cleanup)
    C inner -> case inner of
        X            -> "CNOT"
        Tensor One X -> "CNOT" -- Specifically catches the (Tensor One X) case
        Tensor X One -> "CNOT"
        _            -> "C(" ++ ppClifford inner ++ ")"

    -- Recursive Tensor cleanup: removes 'One' to reduce noise
    Tensor One b -> ppClifford b
    Tensor a One -> ppClifford a
    Tensor a b   -> ppClifford a ++ " ⊗ " ++ ppClifford b
    
    -- Composition and Adjoints
    Compose a b  -> ppClifford a ++ " . " ++ ppClifford b
    Permute ks   -> "Perm " ++ show ks
    Adjoint a    -> "(" ++ ppClifford a ++ ")†"
    
    -- Rotation formatting (for the printOpItem fallback)
    R pauli theta -> printf "R(%s, %.3f)" (ppPauli pauli) theta
    
    _ -> show op

-- | Transforms a Compose tree into a flat list (time sequence)
flattenCompose :: QOp -> [QOp]
flattenCompose (Compose outer inner) = flattenCompose outer ++ flattenCompose inner
flattenCompose op 
    | isIdentity op = [] 
    | otherwise = [op]
  where 
    isIdentity I = True
    isIdentity One = True
    isIdentity (Tensor a b) = isIdentity a && isIdentity b
    isIdentity _ = False

-- | Visualization Engine
visualizeOutput :: QOp -> IO ()
visualizeOutput inputOp = do
    putStrLn "CIRCUIT (Time Sequence):"
    let initialList = reverse $ flattenCompose inputOp 
    if null initialList 
        then putStrLn "      (Empty)"
        else mapM_ printOpItem (zip ([1..] :: [Int]) initialList)
  where
    printOpItem (i, o) = case o of
        -- Special handling for Rotation gates to keep columns aligned
        R pauli theta -> printf "   %2d. R( %-10s )  θ = %6.3f\n" i (ppPauli pauli) theta
        -- Cleaned up Clifford/Tensor output
        _             -> printf "   %2d. %s\n" i (ppClifford o)