module HQP.Opt.GateDecomposition where

import HQP.PrettyPrint
import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions 
import Data.List (sort)
import HQP.QOp.Symplectic 

-- Utility for pretty printing
pp :: QOp -> IO ()
pp = visualizeOutput



-- ==========================================
-- 1. PAULI GADGET COMPONENTS (todo: move to helper?)
-- ==========================================

toZBasis :: QOp -> QOp
toZBasis X = H
toZBasis Y = R X (pi/2) 
toZBasis Z = I          
toZBasis I = I
toZBasis (Tensor a b) = Tensor (toZBasis a) (toZBasis b) 
toZBasis op = op        

fromZBasis :: QOp -> QOp
fromZBasis X = H        
fromZBasis Y = R X (-pi/2) 
fromZBasis Z = I
fromZBasis I = I
fromZBasis (Tensor a b) = Tensor (fromZBasis a) (fromZBasis b)
fromZBasis op = op




-- ==========================================
-- 2. LADDER CONSTRUCTION
-- ==========================================

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

targetRot :: [Int] -> Int -> RealT -> QOp
targetRot activeIndices totalSize theta =
    let 
        targetIdx = last activeIndices
        pre  = nId targetIdx
        post = nId (totalSize - targetIdx - 1)
    in 
        Tensor pre (Tensor (R Z theta) post)














-- ==========================================
-- 3. EXPANSION LOGIC
-- ==========================================

expandPauliRot :: (QOp, QOp, QOp) -> QOp
expandPauliRot (post, rot, pre) = Compose post (Compose rot pre)

expandAllPauliGadgets :: QOp -> QOp
expandAllPauliGadgets op = case op of
    R pauli theta -> expandPauliRot (decomposePauliRotTuple $ R pauli theta)
    Tensor a b    -> Tensor (expandAllPauliGadgets a) (expandAllPauliGadgets b)
    Compose a b   -> Compose (expandAllPauliGadgets a) (expandAllPauliGadgets b)
    DirectSum a b -> DirectSum (expandAllPauliGadgets a) (expandAllPauliGadgets b)
    C x           -> C (expandAllPauliGadgets x)
    Adjoint x     -> Adjoint (expandAllPauliGadgets x)
    _             -> op

decomposePauliRotTuple :: QOp -> (QOp, QOp, QOp)
decomposePauliRotTuple (R Z theta) = (I, R Z theta, I)
decomposePauliRotTuple (R X theta) = (H, R Z theta, H)
decomposePauliRotTuple (R Y theta) = (Adjoint (R X (pi/2)), R Z theta, R X (pi/2))
decomposePauliRotTuple (R pauliString theta) =
    let 
        n = sizeOf pauliString
        opsList = flattenOps pauliString
        activeIndices = [i | (i, op) <- zip [0..] opsList, op /= I]
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

-- | Takes a list of Rotations (assumed to be in execution order), decomposes them,
-- and pushes the 'post' Clifford part forward through the subsequent rotations.
decomposeAndPush :: [QOp] -> QOp
decomposeAndPush rotations = 
    let 
        -- Iterate through the rotations maintaining the accumulated Clifford
        -- (accCliff, processedOps) starts with (One, [])
        (finalCliff, reversedOps) = foldl processStep (One, []) rotations
        
        processStep :: (QOp, [QOp]) -> QOp -> (QOp, [QOp])
        processStep (accCliff, accOps) rot = 
            let 
                -- 1. Apply the inverse of the moving Clifford to the current rotation.
                -- Logic: We have (Rot * AccCliff). We want to move AccCliff to the left (future).
                -- Identity: Rot * AccCliff = AccCliff * (AccCliff_dag * Rot * AccCliff)
                -- So the new effective rotation is: AccCliff_dag * Rot * AccCliff
                -- Adjoint used for the same reason as in splitCliffords.
                effRot = applyCliffordToRotation (Adjoint accCliff) rot

                -- 2. Decompose the effective rotation into (Post * Core * Pre)
                (post, core, pre) = decomposePauliRotTuple effRot

                -- 3. Construct the operation that stays in place (Core * Pre)
                -- Note: 'Compose a b' applies 'b' then 'a'.
                stepOp = Compose core pre 

                -- 4. Update the accumulated Clifford.
                -- The 'post' part is generated physically 'after' the core.
                -- So it joins the AccCliff. 
                -- Sequence: AccCliff_old * Post
                nextCliff = Compose accCliff post
            in 
                (nextCliff, stepOp : accOps)
    in 
        -- Reconstruct the full circuit.
        -- foldl produced a reversed list of operations [OpN, ..., Op1].
        -- The final structure is: FinalCliff * OpN * ... * Op1
        foldl Compose finalCliff reversedOps



liftLeft :: QOp -> Int -> QOp
liftLeft _ 0 = One 
liftLeft (R pauli theta) extraSize = R (Tensor pauli (nId extraSize)) theta
liftLeft op extraSize = Tensor op (nId extraSize)


liftRight :: Int -> QOp -> QOp
liftRight 0 _ = One
liftRight extraSize (R pauli theta) = R (Tensor (nId extraSize) pauli) theta
liftRight extraSize op = Tensor (nId extraSize) op

extractOpList :: QOp -> [QOp]
extractOpList op = case op of
    Compose outer inner -> extractOpList inner ++ extractOpList outer
    
    Tensor a b -> 
        let 
            sizeA = sizeOf a
            sizeB = sizeOf b
            opsA = extractOpList a
            opsB = extractOpList b
            liftedA = map (\o -> liftLeft o sizeB) opsA
            liftedB = map (\o -> liftRight sizeA o) opsB
        in 
            liftedA ++ liftedB

    One -> []
    I   -> [] 
    _ -> [op]

optimizeCircuit :: QOp -> QOp
optimizeCircuit circuit = decomposeAndPush (extractOpList circuit)