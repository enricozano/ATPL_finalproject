module HQP.Opt.GateDecomposition where

import HQP.PrettyPrint
import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions 
import Data.List (sort)

-- Utility for pretty printing
pp :: QOp -> IO ()
pp = putStrLn . showOp 



-- ==========================================
-- 2. PAULI GADGET COMPONENTS
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

buildSmartLadder :: [Int] -> Int -> QOp
buildSmartLadder activeIndices totalSize 
    | length activeIndices < 2 = One
    | otherwise = 
        let pairs = zip activeIndices (tail activeIndices)
        in foldl1 Compose [genericCNOT src tgt totalSize | (src, tgt) <- pairs]

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
decomposePauliRotTuple (R Y theta) = 
    let pre  = R X (pi/2)
        post = Adjoint pre
    in (post, R Z theta, pre)

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
                ladder   = buildSmartLadder activeIndices n
                
                unladder 
                    | length activeIndices < 2 = One
                    | otherwise = 
                        let pairs = reverse (zip activeIndices (tail activeIndices))
                        in foldl1 Compose [genericCNOT src tgt n | (src, tgt) <- pairs]
                
                core = targetRot activeIndices n theta
                
                preCircuit = Compose unladder postOps
                
                postCircuit = Compose preOps ladder
                
            in (postCircuit, core, preCircuit)

decomposePauliRotTuple op = (One, op, One)