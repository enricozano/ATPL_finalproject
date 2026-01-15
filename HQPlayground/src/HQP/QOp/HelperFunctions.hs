module HQP.QOp.HelperFunctions where

import HQP.QOp.Syntax
import Data.Bits(FiniteBits,countTrailingZeros)

-- =========================================================================
--  1. BITWISE AND MATHEMATICAL UTILITIES
-- =========================================================================

-- Description: Converts an integer to a list of bits (binary representation).
-- Inputs: An integral number.
-- Outputs: A list of binary digits (reverse order of standard writing, LSB last).
toBits :: (Integral a) => a -> [a]
toBits 0 = []
toBits k = (toBits (k `div` 2)) ++ [(k `mod` 2)]

-- Description: Converts an integer to a binary list padded with zeros to a specific length.
-- Inputs: Desired length n, and the integer k to convert.
-- Outputs: A list of n integers (0 or 1).
toBits' :: Int -> Int -> [Int]
toBits' n k = let 
    bits = toBits k
    m    = length bits
  in
    (replicate (n-m) 0) ++ bits

-- Description: Calculates the integer base-2 logarithm (or counting trailing zeros).
-- Inputs: A number.
-- Outputs: The integer log2.
ilog2 :: (FiniteBits a, Integral a) => a -> Int
ilog2 = countTrailingZeros

-- Description: Performs a Boolean XOR operation.
-- Inputs: Two booleans.
-- Outputs: True if inputs differ, False otherwise.
bxor :: Bool -> Bool -> Bool
bxor a b = a /= b


-- =========================================================================
--  2. CIRCUIT DIMENSIONS AND PROPERTIES
-- =========================================================================

-- Description: Calculates the output dimension (range) of an operator.
-- Inputs: A Quantum Operator.
-- Outputs: Number of qubits in the output space.
op_range :: QOp -> Int
op_range op = case op of
    One          -> 0
    Ket ks        -> length ks 
    C a           -> 1 + op_range a
    Tensor    a b -> op_range a + op_range b
    DirectSum a b -> let 
      (w_a, w_b) = (op_range a, op_range b)
      in 
        if w_a == w_b then 1+w_a 
        else 
          error $ "Direct sum of incompatible operator dimensions: " 
          ++ show w_a ++ " qubits /= " ++ show w_b ++ " qubits."

    Compose   a b -> max (op_range a) (op_range b)
    Adjoint   a   -> op_domain a
    Permute   ks  -> length ks 
    _             -> 1 -- 1-qubit gates

-- Description: Calculates the input dimension (domain) of an operator.
-- Inputs: A Quantum Operator.
-- Outputs: Number of qubits in the input space.
op_domain :: QOp -> Int
op_domain op = case op of
    One          -> 0
    Ket _        -> 0 
    C a           -> 1 + op_domain a
    Tensor    a b -> op_domain a + op_domain b
    DirectSum a b -> let 
      (w_a, w_b) = (op_domain a, op_domain b)
      in 
        if w_a == w_b then 1+w_a 
        else 
          error $ "Direct sum of incompatible operator dimensions: " 
          ++ show w_a ++ " qubits /= " ++ show w_b ++ " qubits."
    Compose   a b -> max (op_domain a) (op_domain b)
    Adjoint   a   -> op_range a
    Permute   ks  -> length ks 
    _             -> 1 -- 1-qubit gates

-- Description: Calculates the size (width) of an operator. similar to op_range but generally used for square operators.
-- Inputs: A Quantum Operator.
-- Outputs: The qubit width.
sizeOf :: QOp -> Int
sizeOf op = case op of
    I            -> 1
    X            -> 1
    Y            -> 1
    Z            -> 1
    H            -> 1
    SX           -> 1
    C sub        -> 1 + sizeOf sub
    Tensor a b   -> sizeOf a + sizeOf b
    Compose a _  -> sizeOf a
    Permute ks   -> length ks
    Adjoint a    -> sizeOf a
    R a _        -> sizeOf a
    One          -> 0
    _            -> 1 

-- Description: Calculates the dimension of a single program step (Unitary or Measure).
-- Inputs: A Step.
-- Outputs: Integer dimension.
step_range :: Step -> Int
step_range step = case step of 
  Unitary op -> op_range op
  Measure ks -> 1 + maximum ks

-- Description: Calculates the maximum dimension required by a full program.
-- Inputs: A Program.
-- Outputs: Integer dimension (max width).
prog_range :: Program -> Int
prog_range program = maximum $ map step_range program


-- =========================================================================
--  3. CIRCUIT CONSTRUCTION AND GENERATORS
-- =========================================================================

-- Description: Generates a tensor product of n Identity gates.
-- Inputs: Number of qubits n.
-- Outputs: Identity operator of size n.
nId :: Int -> QOp
nId 0 = One
nId 1 = I
nId n = Tensor I (nId (n - 1))

-- Description: Constructs a CNOT gate between arbitrary source and target qubits in a system of size n.
--              Handles distant qubits by adding Swap/Hadamard networks if necessary.
-- Inputs: Source index, Target index, Total system size n.
-- Outputs: A Quantum Operator representing the generic CNOT.
genericCNOT :: Int -> Int -> Int -> QOp
genericCNOT src tgt n 
    | src == tgt = error "Source and Target cannot be the same"
    | src < 0 || src >= n = error "Source index out of bounds"
    | tgt < 0 || tgt >= n = error "Target index out of bounds"
    | src > tgt  = 
        let 
            flippedCNOT = genericCNOT tgt src n
            padPre   = nId tgt
            padMid   = nId (src - tgt - 1)
            padPost  = nId (n - src - 1)
            
            hLayer = Tensor padPre (Tensor H (Tensor padMid (Tensor H padPost)))
        in 
            Compose hLayer (Compose flippedCNOT hLayer)

    | otherwise  = 
        let 
            gapSize = tgt - src - 1 
            remoteGate = C (Tensor (nId gapSize) X)
            prePad  = nId src
            postPad = nId (n - tgt - 1)
        in 
            Tensor prePad (Tensor remoteGate postPad)


-- =========================================================================
--  4. STRUCTURAL MANIPULATION AND FLATTENING
-- =========================================================================

-- Description: Extends an operator to act on a larger Hilbert space by tensoring Identity on the right (increasing index count).
-- Inputs: The operator to lift and the number of extra qubits to add.
-- Outputs: The lifted Quantum Operator.
liftLeft :: QOp -> Int -> QOp
liftLeft _ 0 = One 
liftLeft (R pauli theta) extraSize = R (Tensor pauli (nId extraSize)) theta
liftLeft op extraSize = Tensor op (nId extraSize)

-- Description: Extends an operator to act on a larger Hilbert space by tensoring Identity on the left (shifting indices).
-- Inputs: The number of extra qubits to add and the operator to lift.
-- Outputs: The lifted Quantum Operator.
liftRight :: Int -> QOp -> QOp
liftRight 0 _ = One
liftRight extraSize (R pauli theta) = R (Tensor (nId extraSize) pauli) theta
liftRight extraSize op = Tensor (nId extraSize) op

-- Description: Flattens a pure Tensor tree into a list of single-qubit (or atomic) operators.
-- Inputs: A Tensor operator.
-- Outputs: A list of QOp.
flattenOps :: QOp -> [QOp]
flattenOps (Tensor a b) = flattenOps a ++ flattenOps b
flattenOps op 
    | sizeOf op == 1 = [op]
    | otherwise      = error "Complex operators inside Tensor not supported in flattening yet"

-- Description: Flattens a complex nested Quantum Operator (Compose/Tensor) into a linear list of operations lifted to the full system size.
-- Inputs: A Quantum Operator.
-- Outputs: A list of Quantum Operators.
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

-- Description: Recursively searches for the index of the first 'X' operator in a tensor chain.
--              Used to identify the target qubit in Controlled gates.
-- Inputs: A Quantum Operator.
-- Outputs: Just index (int) if found, Nothing otherwise.
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

-- =========================================================================
--  5. CIRCUIT ANALYSIS
-- =========================================================================

-- Description: Returns a list of indices where a Rotation operator acts non-trivially (non-Identity).
-- Inputs: A Rotation operator.
-- Outputs: A list of integer indices.
getActiveIndices :: QOp -> [Int]
getActiveIndices (R pauli _) = 
    [i | (i, p) <- zip [0..] (flattenOps pauli), p /= I]
getActiveIndices _ = []