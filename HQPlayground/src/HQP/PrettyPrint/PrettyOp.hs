module HQP.PrettyPrint.PrettyOp where

import HQP.QOp.Syntax
import Data.List (intercalate)
import Text.Printf (printf)

-- Description: Formats a Pauli string component, removing 'One' (Identity) noise for cleaner output.
-- Inputs: A Quantum Operator (usually a tensor product of Paulis).
-- Outputs: A formatted String (e.g., "X Y Z").
ppPauli :: QOp -> String
ppPauli op = case op of
    I          -> "I"
    X          -> "X"
    Y          -> "Y"
    Z          -> "Z"
    Tensor a b -> ppPauli a ++ " " ++ ppPauli b
    One        -> ""
    _          -> "?"

-- Description: Converts a Quantum Operator into a human-readable string. 
--              Performs recursive cleanup to simplify Tensors with Identity 
--              and formats special gates (e.g., CNOT, Clifford Rotations).
-- Inputs: A Quantum Operator.
-- Outputs: A formatted String representing the operator.
showOp :: QOp -> String
showOp op = case op of
    I   -> "I"
    X   -> "X"
    Y   -> "Y"
    Z   -> "Z"
    H   -> "H"
    SX  -> "SX"
    One -> "I" 
    
    R X theta 
        | abs (theta - (pi/2)) < 0.0001 -> "R X(π/2)"
        | abs (theta + (pi/2)) < 0.0001 -> "R X(-π/2)"
    
    C inner -> case inner of
        X            -> "CNOT"
        Tensor One X -> "CNOT" 
        Tensor X One -> "CNOT"
        _            -> "C(" ++ showOp inner ++ ")"

    Tensor One b -> showOp b
    Tensor a One -> showOp a
    Tensor a b   -> showOp a ++ " ⊗ " ++ showOp b
    
    DirectSum a b -> "(" ++ showOp a ++ " ⊕ " ++ showOp b ++ ")"

    Compose a b  -> showOp a ++ " . " ++ showOp b
    
    Permute ks   -> "Perm " ++ show ks
    Bra ks       -> "Bra " ++ show ks
    
    R pauli theta -> printf "R(%s, %.3f)" (showOp pauli) theta
    
    Adjoint a    -> case a of
        Tensor x y    -> showOp (Tensor (mkAdj x) (mkAdj y))
        Compose x y   -> showOp (Compose (mkAdj y) (mkAdj x)) 
        DirectSum x y -> showOp (DirectSum (mkAdj x) (mkAdj y))
        C x           -> showOp (C (mkAdj x))
        
        Adjoint x     -> showOp x
        
        R pauli theta -> showOp (R pauli (-theta))
        
        I             -> "I"
        One           -> "I"
        X             -> "X"
        Y             -> "Y"
        Z             -> "Z"
        H             -> "H"
        
        Ket ks        -> "Bra " ++ show ks
        _             -> "Adj(" ++ showOp a ++ ")"

    _ -> show op
  where
    mkAdj :: QOp -> QOp
    mkAdj One = One
    mkAdj I   = I
    mkAdj X   = X
    mkAdj Y   = Y
    mkAdj Z   = Z
    mkAdj H   = H
    mkAdj (R p t) = R p (-t)
    mkAdj (Adjoint x) = x
    mkAdj x   = Adjoint x


-- Description: Prints the string representation of an operator to stdout.
-- Inputs: A Quantum Operator.
-- Outputs: IO action.
printOp :: QOp -> IO ()
printOp = putStrLn . showOp

-- Description: Formats a single program step (Unitary or Measurement).
-- Inputs: A Step.
-- Outputs: A String description.
showStep :: Step -> String
showStep (Measure ks) = "Measure " ++ show ks
showStep (Unitary op) = "Unitary $ " ++ showOp op

-- Description: Formats a full quantum program into a line-by-line string.
-- Inputs: A Program (list of Steps).
-- Outputs: A String representation of the entire program.
showProgram :: Program -> String
showProgram steps = intercalate "\n" 
    [ "step" ++ show i ++ " = " ++ showStep step
    | (i :: Int, step) <- zip [1..] steps
    ]


-- Description: Flattens a nested Compose tree into a linear list of operators (time sequence).
-- Inputs: A composed Quantum Operator (e.g., A . B . C).
-- Outputs: A list of Quantum Operators [A, B, C].
flattenCompose :: QOp -> [QOp]
flattenCompose (Compose outer inner) = flattenCompose outer ++ flattenCompose inner
flattenCompose op 
    | isIdentity op = [] 
    | otherwise = [op]
  where 
    isIdentity I = True
    isIdentity One = True
    isIdentity (Tensor a b) = isIdentity a && isIdentity b
    isIdentity _ = False

-- Description: Prints a time-sequenced visual trace of the circuit to the console.
--              Distinguishes between standard gates and specific rotations for clearer output.
-- Inputs: A Quantum Operator representing the full circuit.
-- Outputs: IO action that prints the circuit trace.
visualizeOutput :: QOp -> IO ()
visualizeOutput inputOp = do
    putStrLn "CIRCUIT (Time Sequence):"
    let initialList = reverse $ flattenCompose inputOp 
    if null initialList 
        then putStrLn "      (Empty)"
        else mapM_ printOpItem (zip ([1..] :: [Int]) initialList)
  where
    printOpItem (i, o) = case o of
        R pauli theta -> 
            if pauli == X && (abs (theta - (pi/2)) < 0.0001 || abs (theta + (pi/2)) < 0.0001)
            then printf "   %2d. %s\n" i (showOp o)
            else printf "   %2d. R( %-10s )  θ = %6.3f\n" i (showOp pauli) theta
        _ -> printf "   %2d. %s\n" i (showOp o)