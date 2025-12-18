module HQP.Opt.GateDecomposition where

import HQP.PrettyPrint
import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions 
import Data.List (sort)

-- Utility for pretty printing
pp :: QOp -> IO ()
pp = putStrLn . showOp 

-- ==========================================
-- 1. HELPER GATES
-- ==========================================

genericCNOT :: Int -> Int -> Int -> QOp
genericCNOT src tgt n 
    | src == tgt = error "Source and Target cannot be the same"
    | src > tgt  = error "This simplified impl supports src < tgt only for now"
    | otherwise  = 
        let 
            gapSize = tgt - src - 1 
            remoteGate = C (Tensor (nId gapSize) X)
            prePad  = nId src
            postPad = nId (n - tgt - 1)
        in 
            Tensor prePad (Tensor remoteGate postPad)

flattenOps :: QOp -> [QOp]
flattenOps (Tensor a b) = flattenOps a ++ flattenOps b
flattenOps op 
    | sizeOf op == 1 = [op]
    | otherwise      = error "Complex operators inside Tensor not supported in flattening yet"

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

expandPauliRot :: QOp -> QOp
expandPauliRot (R Z theta) = R Z theta 
expandPauliRot (R X theta) = Compose H (Compose (R Z theta) H)
expandPauliRot (R Y theta) = 
    let sGate = R Z (pi/2); sDag = Adjoint sGate
    in Compose sDag (Compose H (Compose (R Z theta) (Compose H sGate)))

expandPauliRot (R pauliString theta) =
    let 
        n = sizeOf pauliString
        opsList = flattenOps pauliString
        activeIndices = [i | (i, op) <- zip [0..] opsList, op /= I]
        preOps  = toZBasis pauliString
        postOps = fromZBasis pauliString
    in
        if null activeIndices then nId n 
        else 
            let
                ladder   = buildSmartLadder activeIndices n
                unladder 
                    | length activeIndices < 2 = One
                    | otherwise = 
                        let pairs = reverse (zip activeIndices (tail activeIndices))
                        in foldl1 Compose [genericCNOT src tgt n | (src, tgt) <- pairs]
                core = targetRot activeIndices n theta
            in
                Compose preOps 
                  (Compose ladder 
                    (Compose core 
                      (Compose unladder postOps)))

expandPauliRot op = op

expandAllPauliGadgets :: QOp -> QOp
expandAllPauliGadgets op = case op of
    R pauli theta -> expandPauliRot (R pauli theta)
    Tensor a b    -> Tensor (expandAllPauliGadgets a) (expandAllPauliGadgets b)
    Compose a b   -> Compose (expandAllPauliGadgets a) (expandAllPauliGadgets b)
    DirectSum a b -> DirectSum (expandAllPauliGadgets a) (expandAllPauliGadgets b)
    C x           -> C (expandAllPauliGadgets x)
    Adjoint x     -> Adjoint (expandAllPauliGadgets x)
    _             -> op