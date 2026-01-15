module HQP.QOp.Symplectic where

import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions

-- =========================================================================
--  1. DATA TYPES AND REPRESENTATIONS
-- =========================================================================

-- | Symplectic representation of a Pauli String.
-- sign = True represents -1, False represents +1.
data Symplectic = Symp {
    xs   :: [Bool],
    zs   :: [Bool],
    sign :: Bool 
} deriving (Show, Eq)


-- =========================================================================
--  2. CONVERSIONS (PAULI <-> SYMPLECTIC)
-- =========================================================================

-- Description: Converts a Pauli Operator (tensor structure) into its Symplectic vector representation.
--              Maps I->(0,0), X->(1,0), Z->(0,1), Y->(1,1).
-- Inputs: A Quantum Operator (must be a valid Pauli string).
-- Outputs: A Symplectic data structure.
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

-- Description: Converts a Symplectic vector back into a Pauli Operator string.
-- Inputs: A Symplectic data structure.
-- Outputs: A Quantum Operator (Tensor chain of Paulis).
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


-- =========================================================================
--  3. CORE LOGIC: CLIFFORD CONJUGATION
-- =========================================================================

-- Description: Recursively conjugates a Pauli string (symp) by a Clifford operator.
--              Effectively computes: C * P * C†.
--              Updates the X and Z bits and the phase (sign) according to symplectic rules.
-- Inputs: A Clifford Operator (C) and the Symplectic representation of a Pauli (P).
-- Outputs: The Symplectic representation of the resulting Pauli (P').
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
        case findXIndex inner of
            Just idx -> 
                let 
                    targetAbs = idx + 1 
                    
                    xc = xs symp !! 0
                    zc = zs symp !! 0
                    xt = xs symp !! targetAbs
                    zt = zs symp !! targetAbs
                    
                    xt_new = xt `bxor` xc 
                    zc_new = zc `bxor` zt
                    phase_flip = xc && zt && (not (xt `bxor` zc))
                    
                    replaceAt i val list = take i list ++ [val] ++ drop (i+1) list
                    
                    xs_final = replaceAt targetAbs xt_new (xs symp)
                    zs_final = replaceAt 0 zc_new (zs symp)
                    s_final  = (sign symp) `bxor` phase_flip
                in
                    Symp xs_final zs_final s_final
            Nothing -> symp 

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


-- =========================================================================
--  4. HIGH-LEVEL API
-- =========================================================================

-- Description: High-level function to conjugate a full rotation gate R(P, theta) by a Clifford C.
--              Result: R(C · P · C†, ±theta).
-- Inputs: A Clifford Operator C and the Rotation Operator R(P, theta).
-- Outputs: The transformed Rotation Operator.
applyCliffordToRotation :: QOp -> QOp -> QOp
applyCliffordToRotation cliff op = case op of
    R pauli theta -> 
        let sOut = applyCliffordRecursive cliff (pauliToSymp pauli)
        in R (sympToPauli sOut) (if sign sOut then -theta else theta)
    Compose a b -> Compose (applyCliffordToRotation cliff a) (applyCliffordToRotation cliff b)
    Tensor a b  -> Tensor  (applyCliffordToRotation cliff a) (applyCliffordToRotation cliff b)
    _ -> op


