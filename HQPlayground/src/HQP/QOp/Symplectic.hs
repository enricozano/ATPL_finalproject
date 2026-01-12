module HQP.QOp.Symplectic where

import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions

-- | Symplectic representation of a Pauli String.
-- sign = True represents -1, False represents +1.
data Symplectic = Symp {
    xs   :: [Bool],
    zs   :: [Bool],
    sign :: Bool 
} deriving (Show, Eq)

-- | Convert a Pauli operator to its Symplectic vector.
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
    _ -> error $ "pauliToSymp: Operator " ++ show op ++ " is not a valid Pauli String."

-- | Convert a Symplectic vector back to a Pauli operator string.
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
sympToPauli _ = error "sympToPauli: inconsistent vector lengths"

findXIndex :: QOp -> Maybe Int
findXIndex op = case op of
    X -> Just 0
    Tensor a b -> 
        case findXIndex a of
            Just k -> Just k
            Nothing -> case findXIndex b of
                Just k -> Just (sizeOf a + k)
                Nothing -> Nothing
    I -> Nothing
    One -> Nothing
    _ -> Nothing

-- | Conjugates a Pauli string (symp) by a Clifford operator (cliff).
--   Computes C * P * C_dagger
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
        R axis theta -> applyCliffordRecursive (R axis (-theta)) symp
        Adjoint x -> applyCliffordRecursive x symp
        _ -> applyCliffordRecursive op symp 

    C inner -> 
        -- FIX: General handling for C(Operator) using recursive search for X target.
        -- Qubit 0 is always Control. Target is at (1 + index of X in inner).
        case findXIndex inner of
            Just idx -> 
                let 
                    targetAbs = idx + 1 
                    
                    -- Extract bits
                    xc = xs symp !! 0
                    zc = zs symp !! 0
                    xt = xs symp !! targetAbs
                    zt = zs symp !! targetAbs
                    
                    -- CNOT Logic: 
                    -- X_target flips if X_control is 1
                    -- Z_control flips if Z_target is 1
                    xt_new = xt `bxor` xc 
                    zc_new = zc `bxor` zt
                    phase_flip = xc && zt && (not (xt `bxor` zc))
                    
                    -- Helper to update list
                    replaceAt i val list = take i list ++ [val] ++ drop (i+1) list
                    
                    -- Apply updates
                    xs_final = replaceAt targetAbs xt_new (xs symp)
                    zs_final = replaceAt 0 zc_new (zs symp)
                    s_final  = (sign symp) `bxor` phase_flip
                in
                    Symp xs_final zs_final s_final
            Nothing -> symp -- Treat as Identity if no X found (e.g. C(I))

    R X theta 
        | abs (theta - (pi/2)) < 0.0001 -> 
            let (x, z) = (head (xs symp), head (zs symp))
                x_new = x `bxor` z
                z_new = z
                s_new = if (not x) && z then not (sign symp) else sign symp
            in Symp [x_new] [z_new] s_new

        | abs (theta + (pi/2)) < 0.0001 -> 
            let (x, z) = (head (xs symp), head (zs symp))
                x_new = x `bxor` z
                z_new = z
                s_new = if x && z then not (sign symp) else sign symp
            in Symp [x_new] [z_new] s_new
        
        | otherwise -> symp
    H -> let (x, z) = (head (xs symp), head (zs symp)) in Symp [z] [x] (if x && z then not (sign symp) else sign symp)
    X -> let z = head (zs symp) in if z then symp { sign = not (sign symp) } else symp
    Z -> let x = head (xs symp) in if x then symp { sign = not (sign symp) } else symp
    Y -> let (x, z) = (head (xs symp), head (zs symp)) in if x /= z then symp { sign = not (sign symp) } else symp
    I -> symp; One -> symp
    _ -> symp 

-- | High-level function to conjugate a rotation by a Clifford.
--   C . R(P, theta) . C_dagger = R(C.P.C_dag, theta)
applyCliffordToRotation :: QOp -> QOp -> QOp
applyCliffordToRotation cliff op = case op of
    R pauli theta -> 
        let sOut = applyCliffordRecursive cliff (pauliToSymp pauli)
        in R (sympToPauli sOut) (if sign sOut then -theta else theta)
    Compose a b -> Compose (applyCliffordToRotation cliff a) (applyCliffordToRotation cliff b)
    Tensor a b  -> Tensor  (applyCliffordToRotation cliff a) (applyCliffordToRotation cliff b)
    _ -> op


-- note: would be smart to convert the full circuit into symplectic form and optimize there