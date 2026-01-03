module HQP.Opt.CliffordExtraction where

import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions
import HQP.QOp.Symplectic 

-- | Check if an operator is a Clifford. (not complete, but sufficient for our purposes)
isClifford :: QOp -> Bool
isClifford op = case op of
    R X theta -> abs (abs theta - (pi/2)) < 0.0001
    R _ _ -> False
    Compose a b -> isClifford a && isClifford b
    Tensor a b -> isClifford a && isClifford b
    Adjoint a -> isClifford a
    _ -> True 

-- | Algorithm: Pushes all Cliffords
splitCliffords :: QOp -> (QOp, QOp)
splitCliffords op 
    | isClifford op = (op, nId (sizeOf op))
    | otherwise = case op of
        R pauli theta -> (nId (sizeOf pauli), R pauli theta)
        Compose a b -> 
            let (cliffA, rotA) = splitCliffords a
                (cliffB, rotB) = splitCliffords b
                rotA_trans = applyCliffordToRotation (Adjoint cliffB) rotA
            in (Compose cliffA cliffB, Compose rotA_trans rotB)
        Tensor a b -> 
            let (ca, ra) = splitCliffords a
                (cb, rb) = splitCliffords b
            in (Tensor ca cb, Tensor ra rb)
        _ -> (nId (sizeOf op), op)

pushCliffords :: QOp -> QOp
pushCliffords op = 
    let (cliffs, rots) = splitCliffords op
    in Compose cliffs rots