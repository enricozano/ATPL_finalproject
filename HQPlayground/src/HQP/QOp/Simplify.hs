module HQP.QOp.Simplify where
import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions
import Data.Function (fix)

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f = fix (\rec x ->
    let x' = f x
    in if x' == x then x else rec x')

-- | One is the neutral element for Tensor and Compose. This function removes all redundant occurrences of One in a QOp expression. 
cleanOnes :: QOp -> QOp
cleanOnes op = case op of
    -- Simplification rules
    C One               -> One
    R One _             -> One
    Tensor  One b       -> cleanOnes b
    Tensor  a     One   -> cleanOnes a
    Compose One b       -> cleanOnes b
    Compose a     One   -> cleanOnes a
    Compose a     I     -> cleanOnes a
    Compose I     a     -> cleanOnes a
    -- Below we just recurse. 
    Tensor  a b           -> Tensor    (cleanOnes a) (cleanOnes b)
    DirectSum a b         -> DirectSum (cleanOnes a) (cleanOnes b)
    Compose a b           -> Compose   (cleanOnes a) (cleanOnes b)
    Adjoint a             -> Adjoint   (cleanOnes a)
    C a                   -> C         (cleanOnes a)
    R a phi              -> R         (cleanOnes a) phi
    -- Rest of constructors are atomsø
    _                     -> op

  
-- simplifyPass :: [o -> o] -> o -> o
-- simplifyPass rewriterules op    = foldr (\f -> f op) op rewriterules

-- simplifyFixpoint :: [o -> o] -> o -> o
-- simplifyFixpoint rewriterules op = fixpoint (simplifyPass rewriterules) op

-- | rectangularize take a general QOp syntax tree and outputs a "rectangular" syntax tree on the form of a list of Compositions of n-qubit tensor products.
--rectangularize :: QOp -> QOp

    
cleanAdjoints :: QOp -> QOp
cleanAdjoints op = case op of
    Adjoint I                 -> I
    Adjoint X                 -> X
    Adjoint Y                 -> Y
    Adjoint Z                 -> Z
    Adjoint H                 -> H
    Adjoint (Adjoint a)       -> cleanAdjoints a
    --(AB)^-1 = B^-1 A^-1
    Adjoint (Compose a b)     -> Compose (cleanAdjoints (Adjoint b)) (cleanAdjoints (Adjoint a))
    Adjoint (Tensor a b)      -> Tensor  (cleanAdjoints (Adjoint a)) (cleanAdjoints (Adjoint b))
    Adjoint (DirectSum a b)   -> DirectSum (cleanAdjoints (Adjoint a)) (cleanAdjoints (Adjoint b))
    -- Compose a (Adjoint a)     -> I -- A^{-1}A = I 
    -- Compose (Adjoint a) a     -> I -- AA^{-1} = I
    Tensor  a b               -> Tensor    (cleanAdjoints a) (cleanAdjoints b)
    DirectSum a b             -> DirectSum (cleanAdjoints a) (cleanAdjoints b)
    Compose a b               -> Compose   (cleanAdjoints a) (cleanAdjoints b)
    _                         -> op    






