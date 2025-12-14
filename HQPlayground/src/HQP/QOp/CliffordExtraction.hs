module HQP.QOp.CliffordExtraction where

import HQP.QOp.Syntax
import Data.Bits (xor)

-- ==========================================
-- 1. STRUTTURE E PARSING (Invariato)
-- ==========================================

data Symplectic = Symp {
    xs   :: [Bool],
    zs   :: [Bool],
    sign :: Bool 
} deriving (Show, Eq)

bxor :: Bool -> Bool -> Bool
bxor a b = a /= b

sizeOf :: QOp -> Int
sizeOf op = case op of
    I -> 1; X -> 1; Y -> 1; Z -> 1; H -> 1; SX -> 1
    C _ -> 2           
    Tensor a b -> sizeOf a + sizeOf b
    Compose a _ -> sizeOf a
    Permute ks -> length ks
    Adjoint a -> sizeOf a
    R a _ -> sizeOf a
    One -> 0
    _ -> 1 

pauliToSymp :: QOp -> Symplectic
pauliToSymp op = case op of
    I -> Symp [False] [False] False
    X -> Symp [True]  [False] False
    Z -> Symp [False] [True]  False
    Y -> Symp [True]  [True]  False
    Tensor a b -> 
        let sa = pauliToSymp a
            sb = pauliToSymp b
        in Symp (xs sa ++ xs sb) (zs sa ++ zs sb) (sign sa `bxor` sign sb)
    One -> Symp [] [] False 
    _ -> error $ "pauliToSymp: L'operatore " ++ show op ++ " non è una Pauli String valida."

sympToPauli :: Symplectic -> QOp
sympToPauli (Symp [] [] _) = One
sympToPauli (Symp (x:xt) (z:zt) _) = 
    let op = case (x, z) of
            (False, False) -> I
            (True, False)  -> X
            (False, True)  -> Z
            (True, True)   -> Y
        rest = sympToPauli (Symp xt zt False)
    in if null xt then op else Tensor op rest
sympToPauli _ = error "sympToPauli: lunghezza vettori incoerente"

-- ==========================================
-- 2. ENGINE SIMPLETTICO (Invariato)
-- ==========================================

applyCNOTLogic :: Symplectic -> Symplectic
applyCNOTLogic (Symp [xc, xt] [zc, zt] s) =
    let xt_new = xt `bxor` xc 
        zc_new = zc `bxor` zt 
    in Symp [xc, xt_new] [zc_new, zt] s
applyCNOTLogic _ = error "applyCNOTLogic: Richiede 2 qubit"

applyCliffordRecursive :: QOp -> Symplectic -> Symplectic
applyCliffordRecursive cliff symp = case cliff of
    Compose outer inner -> applyCliffordRecursive outer (applyCliffordRecursive inner symp)
    Tensor a b -> 
        let na = sizeOf a
            (xa, xb) = splitAt na (xs symp); (za, zb) = splitAt na (zs symp)
            sA = applyCliffordRecursive a (Symp xa za False)
            sB = applyCliffordRecursive b (Symp xb zb False)
        in Symp (xs sA ++ xs sB) (zs sA ++ zs sB) ((sign symp) `bxor` (sign sA) `bxor` (sign sB))
    Adjoint op -> case op of
        Compose a b -> applyCliffordRecursive (Compose (Adjoint b) (Adjoint a)) symp
        Tensor a b  -> applyCliffordRecursive (Tensor (Adjoint a) (Adjoint b)) symp
        SX -> applyCliffordRecursive SX symp 
        _ -> applyCliffordRecursive op symp 
    C X -> applyCNOTLogic symp
    H -> let (x, z) = (head (xs symp), head (zs symp)) in Symp [z] [x] (if x && z then not (sign symp) else sign symp)
    SX -> let (x, z) = (head (xs symp), head (zs symp)) in Symp [x] [x `bxor` z] (sign symp)
    X -> let z = head (zs symp) in if z then symp { sign = not (sign symp) } else symp
    Z -> let x = head (xs symp) in if x then symp { sign = not (sign symp) } else symp
    Y -> let (x, z) = (head (xs symp), head (zs symp)) in if x /= z then symp { sign = not (sign symp) } else symp
    I -> symp; One -> symp
    _ -> symp 

applyCliffordToRotation :: QOp -> QOp -> QOp
applyCliffordToRotation cliff op = case op of
    R pauli theta -> 
        let sOut = applyCliffordRecursive cliff (pauliToSymp pauli)
        in R (sympToPauli sOut) (if sign sOut then -theta else theta)
    Compose a b -> Compose (applyCliffordToRotation cliff a) (applyCliffordToRotation cliff b)
    Tensor a b  -> Tensor  (applyCliffordToRotation cliff a) (applyCliffordToRotation cliff b)
    _ -> op

-- ==========================================
-- 3. ALGORITMO DI ESTRAZIONE (Modificato)
-- ==========================================

nId :: Int -> QOp
nId 0 = One; nId 1 = I; nId n = Tensor I (nId (n-1))

isClifford :: QOp -> Bool
isClifford op = case op of
    R _ _ -> False
    Compose a b -> isClifford a && isClifford b
    Tensor a b -> isClifford a && isClifford b
    Adjoint a -> isClifford a
    _ -> True 

-- | Estrae (Cliffords, Rotations) spingendo i Clifford a SINISTRA (Futuro)
--   Logica: R . C -> C . R' (dove R' = C_dag . R . C)
splitCliffords :: QOp -> (QOp, QOp)
splitCliffords op 
    | isClifford op = (op, nId (sizeOf op)) -- (Cliff, Id)
    | otherwise = case op of
        R pauli theta -> (nId (sizeOf pauli), R pauli theta) -- (Id, Rot)
        
        Compose a b -> 
            -- a = Futuro (Left), b = Passato (Right)
            -- a = (Ca . Ra), b = (Cb . Rb)
            let (cliffA, rotA) = splitCliffords a
                (cliffB, rotB) = splitCliffords b
                
                -- Sequenza corrente: Ca . Ra . Cb . Rb
                -- Vogliamo scambiare Ra e Cb.
                -- Ra è a sinistra (Futuro), Cb è a destra (Passato).
                -- Vogliamo Cb a sinistra e Ra a destra.
                -- Ra . Cb = Cb . (Cb_dagger . Ra . Cb)
                
                rotA_trans = applyCliffordToRotation (Adjoint cliffB) rotA
                
            -- Nuova Sequenza: (Ca . Cb) . (rotA_trans . Rb)
            in (Compose cliffA cliffB, Compose rotA_trans rotB)

        Tensor a b -> 
            let (ca, ra) = splitCliffords a
                (cb, rb) = splitCliffords b
            in (Tensor ca cb, Tensor ra rb)
            
        _ -> (nId (sizeOf op), op) -- Fallback, assume Rotazione generica

-- | API PUBBLICA: Restituisce Compose Cliffords Rotations
pushCliffords :: QOp -> QOp
pushCliffords op = 
    let (cliffs, rots) = splitCliffords op
    in Compose cliffs rots