module HQP.PrettyPrint.Visualizer where

import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions
import HQP.Opt.CliffordExtraction (isClifford, pushCliffords)
import Text.Printf (printf)

-- | Formatta una stringa di Pauli
ppPauli :: QOp -> String
ppPauli op = case op of
    I -> "I"
    X -> "X"
    Y -> "Y"
    Z -> "Z"
    Tensor a b -> ppPauli a ++ " " ++ ppPauli b
    One -> ""
    _ -> "?"

-- | Formatta un operatore di Clifford
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

-- | Trasforma un albero di Compose in una lista piatta (sequenza temporale)
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

-- | Motore di visualizzazione
visualizeOutput :: QOp -> IO ()
visualizeOutput inputOp = do
    
    putStrLn "CIRCUIT (Time Sequence):"
    let initialList = reverse (flattenCompose inputOp) 
    if null initialList then putStrLn "      (Empty)"
                        else mapM_ printOpItem (zip ([1..] :: [Int]) initialList)

  where
    printOpItem (i, o) = case o of
        R pauli theta -> printf "   %2d. R( %-10s )  θ = %6.3f\n" i (ppPauli pauli) theta
        _             -> printf "   %2d. %s\n" i (ppClifford o)