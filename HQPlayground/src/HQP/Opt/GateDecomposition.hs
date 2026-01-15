module HQP.Opt.GateDecomposition where

import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions 
import HQP.QOp.Symplectic 


-- Description: Maps a Pauli operator to the Z-basis using single-qubit Clifford gates.
-- Inputs: A Pauli operator.
-- Outputs: The corresponding Clifford operator (e.g., H for X, Rx(pi/2) for Y).
toZBasis :: QOp -> QOp
toZBasis X = H
toZBasis Y = R X (pi/2) 
toZBasis Z = I          
toZBasis I = I
toZBasis (Tensor a b) = Tensor (toZBasis a) (toZBasis b) 
toZBasis op = op        

-- Description: The inverse mapping of toZBasis, implemented as the adjoint of the forward transformation.
-- Inputs: A Pauli operator.
-- Outputs: The inverse Clifford operator (Adjoint of toZBasis).
fromZBasis :: QOp -> QOp
fromZBasis op = Adjoint (toZBasis op)

-- Description: Constructs a standard linear CNOT ladder and its inverse for parity computation.
-- Inputs: A list of active qubit indices and the total size of the system.
-- Outputs: A tuple containing the Ladder operator (compute parity) and the Unladder operator (uncompute parity).
buildNaiveLadderPair :: [Int] -> Int -> (QOp, QOp)
buildNaiveLadderPair activeIndices totalSize 
    | length activeIndices < 2 = (One, One)
    | otherwise = 
        let 
            pairs = zip activeIndices (tail activeIndices)
            ops   = [genericCNOT src tgt totalSize | (src, tgt) <- pairs]
            unladder = foldl1 Compose ops
            ladder   = foldl1 Compose (reverse ops)
        in (ladder, unladder)   

-- Description: Creates a Z-rotation on the target qubit (the last active index) within the full system.
-- Inputs: A list of active indices, the total system size, and the rotation angle theta.
-- Outputs: A Quantum Operator representing the rotation.
targetRot :: [Int] -> Int -> RealT -> QOp
targetRot activeIndices totalSize theta =
    let 
        targetIdx = last activeIndices
        pre  = nId targetIdx
        post = nId (totalSize - targetIdx - 1)
    in 
        Tensor pre (Tensor (R Z theta) post)

-- Description: Recombines the decomposed parts of a Pauli gadget into a single operator sequence.
-- Inputs: A tuple of (Post, Core, Pre) operators.
-- Outputs: A composed Quantum Operator.
expandPauliRot :: (QOp, QOp, QOp) -> QOp
expandPauliRot (post, rot, pre) = Compose post (Compose rot pre)

-- Description: Recursively traverses a circuit and expands all Pauli rotation gates into their full gadget decomposition.
-- Inputs: A Quantum Operator.
-- Outputs: The expanded Quantum Operator.
expandAllPauliGadgets :: QOp -> QOp
expandAllPauliGadgets op = case op of
    R pauli theta -> expandPauliRot (decomposePauliRotTuple $ R pauli theta)
    Tensor a b    -> Tensor (expandAllPauliGadgets a) (expandAllPauliGadgets b)
    Compose a b   -> Compose (expandAllPauliGadgets a) (expandAllPauliGadgets b)
    DirectSum a b -> DirectSum (expandAllPauliGadgets a) (expandAllPauliGadgets b)
    C x           -> C (expandAllPauliGadgets x)
    Adjoint x     -> Adjoint (expandAllPauliGadgets x)
    _             -> op

-- Description: Decomposes a Pauli rotation into three logical parts: the basis change/ladder (Pre), the core Z-rotation (Core), and the inverse basis/unladder (Post).
-- Inputs: A Rotation Operator.
-- Outputs: A tuple (Post-Clifford, Core-Rotation, Pre-Clifford).
decomposePauliRotTuple :: QOp -> (QOp, QOp, QOp)
decomposePauliRotTuple (R Z theta) = (I, R Z theta, I)
decomposePauliRotTuple (R X theta) = (H, R Z theta, H)
decomposePauliRotTuple (R Y theta) = (Adjoint (R X (pi/2)), R Z theta, R X (pi/2))
decomposePauliRotTuple (R pauliString theta) =
    let 
        n = sizeOf pauliString
        activeIndices = getActiveIndices (R pauliString theta)
        preOps  = toZBasis pauliString
        postOps = fromZBasis pauliString
    in
        if null activeIndices then (nId n, One, One)
        else 
            let
                (ladder, unladder) = buildNaiveLadderPair activeIndices n
                core = targetRot activeIndices n theta
                postCircuit = Compose postOps unladder
                preCircuit = Compose ladder preOps
            in (postCircuit, core, preCircuit)
decomposePauliRotTuple op = (One, op, One)

-- Description: Takes a list of rotations, decomposes them, and pushes the uncomputation Cliffords forward through subsequent gates.
-- Inputs: A list of Quantum Operators (rotations).
-- Outputs: A single optimized Quantum Operator.
decomposeAndPush :: [QOp] -> QOp
decomposeAndPush rotations = 
    let 
        (finalCliff, reversedOps) = foldl processStep (One, []) rotations
        
        processStep :: (QOp, [QOp]) -> QOp -> (QOp, [QOp])
        processStep (accCliff, accOps) rot = 
            let 
                effRot = applyCliffordToRotation (Adjoint accCliff) rot

                (post, core, pre) = decomposePauliRotTuple effRot

                stepOp = Compose core pre 

                nextCliff = Compose accCliff post
            in 
                (nextCliff, stepOp : accOps)
    in 
        foldl Compose finalCliff reversedOps

-- Description: High-level optimization function that flattens a circuit and applies the decompose-and-push strategy.
-- Inputs: A Quantum Operator representing the full circuit.
-- Outputs: An optimized Quantum Operator.
optimizeCircuit :: QOp -> QOp
optimizeCircuit circuit = decomposeAndPush (extractOpList circuit)