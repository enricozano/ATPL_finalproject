module HQP.Opt.GateDecomposition where

import HQP.PrettyPrint
import HQP.QOp.Syntax
import Data.List (sort)

-- Utility for pretty printing
pp :: QOp -> IO ()
pp = putStrLn . showOp 

-- ==========================================
-- 1. HELPER FUNCTIONS
-- ==========================================

nId :: Int -> QOp
nId 0 = One
nId 1 = I
nId n = Tensor I (nId (n - 1))

sizeOf :: QOp -> Int
sizeOf One = 0
sizeOf I = 1
sizeOf X = 1
sizeOf Y = 1
sizeOf Z = 1
sizeOf H = 1
sizeOf SX = 1
sizeOf (C op) = 1 + sizeOf op
sizeOf (R op _) = sizeOf op
sizeOf (Tensor a b) = sizeOf a + sizeOf b
sizeOf (Compose a _) = sizeOf a
sizeOf _ = 1 

-- | Helper: Converts a generic CNOT between ANY two qubits (source -> target).
--   Essential for skipping over 'I' qubits (e.g., connecting qubit 0 to 2).
genericCNOT :: Int -> Int -> Int -> QOp
genericCNOT src tgt n 
    | src == tgt = error "Source and Target cannot be the same"
    | src > tgt  = error "This simplified impl supports src < tgt only for now (or requires SWAPs)"
    | otherwise  = 
        let 
            -- The gap between control and target
            gapSize = tgt - src - 1 
            -- The gate: Control on top, Gap of identities, Target X
            remoteGate = C (Tensor (nId gapSize) X)
            
            -- Padding before the source and after the target
            prePad  = nId src
            postPad = nId (n - tgt - 1)
        in 
            Tensor prePad (Tensor remoteGate postPad)

-- | Helper: Flattens a Tensor tree into a list of single-qubit operators.
--   Example: Tensor Z (Tensor I X) -> [Z, I, X]
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

-- | NEW: Builds the ladder only connecting the ACTIVE indices.
buildSmartLadder :: [Int] -> Int -> QOp
buildSmartLadder activeIndices totalSize 
    | length activeIndices < 2 = One -- No entanglement needed for 0 or 1 qubit
    | otherwise = 
        -- Creates CNOTs: (idx0->idx1), (idx1->idx2), ...
        let pairs = zip activeIndices (tail activeIndices)
        in foldl1 Compose [genericCNOT src tgt totalSize | (src, tgt) <- pairs]

-- | NEW: Applies rotation specifically on the LAST ACTIVE qubit.
targetRot :: [Int] -> Int -> RealT -> QOp
targetRot activeIndices totalSize theta =
    let 
        targetIdx = last activeIndices
        pre  = nId targetIdx
        post = nId (totalSize - targetIdx - 1)
    in 
        Tensor pre (Tensor (R Z theta) post)

-- ==========================================
-- 3. EXPANSION LOGIC (FIXED)
-- ==========================================

expandPauliRot :: QOp -> QOp

-- Optimized single-qubit cases
expandPauliRot (R Z theta) = R Z theta 
expandPauliRot (R X theta) = Compose H (Compose (R Z theta) H)
expandPauliRot (R Y theta) = 
    let sGate = R Z (pi/2); sDag = Adjoint sGate
    in Compose sDag (Compose H (Compose (R Z theta) (Compose H sGate)))

-- GENERAL CASE: Fixed Logic
expandPauliRot (R pauliString theta) =
    let 
        n = sizeOf pauliString
        
        -- 1. Analyze the string structure
        opsList = flattenOps pauliString
        -- Get indices where the operator is NOT Identity
        -- e.g., [Z, I, Z] -> indices [0, 2]
        activeIndices = [i | (i, op) <- zip [0..] opsList, op /= I]
        
        preOps  = toZBasis pauliString
        postOps = fromZBasis pauliString
        
    in
        -- CASE A: All Identities (e.g., Tensor I I) -> Do nothing (Identity)
        if null activeIndices then nId n 
        
        else 
            let
                -- 2. Build Ladder only between active qubits
                ladder   = buildSmartLadder activeIndices n
                
                -- 3. Unladder is just the reverse
                unladder 
                    | length activeIndices < 2 = One
                    | otherwise = 
                        let pairs = reverse (zip activeIndices (tail activeIndices))
                        in foldl1 Compose [genericCNOT src tgt n | (src, tgt) <- pairs]
                
                -- 4. Rotation applies to the last active qubit found
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

