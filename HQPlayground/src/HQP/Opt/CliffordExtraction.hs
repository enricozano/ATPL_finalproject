module HQP.Opt.CliffordExtraction where

import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions
import HQP.QOp.Symplectic 

-- Description: Determines if a given quantum operator is a Clifford gate. This implementation checks for standard Clifford generators (H, S, CNOT, etc.) and specific rotations (multiples of pi/2).
-- Inputs: A Quantum Operator.
-- Outputs: True if the operator is identified as a Clifford gate, False otherwise.
isClifford :: QOp -> Bool
isClifford op = case op of
    R X theta -> abs (abs theta - (pi/2)) < 0.0001
    R _ _ -> False
    Compose a b -> isClifford a && isClifford b
    Tensor a b -> isClifford a && isClifford b
    Adjoint a -> isClifford a
    _ -> True 

-- Description: Recursively separates a quantum circuit into two components: a trailing Clifford operator and a preceding Rotation layer. It achieves this by commuting Clifford gates past rotations (transforming the rotations in the process).
-- Inputs: A Quantum Operator representing the circuit.
-- Outputs: A tuple (CliffordPart, RotationPart) such that the original circuit is equivalent to 'Compose CliffordPart RotationPart'.
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

-- Description: Optimizes a circuit by pushing all Clifford gates to the end (logically 'after' the rotations).
-- Inputs: A Quantum Operator.
-- Outputs: A new Quantum Operator ordered as (CliffordOps . RotationOps).
pushCliffords :: QOp -> QOp
pushCliffords op = 
    let (cliffs, rots) = splitCliffords op
    in Compose cliffs rots