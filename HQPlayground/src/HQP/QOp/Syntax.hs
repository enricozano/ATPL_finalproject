module HQP.QOp.Syntax where

import Data.Complex

-- =========================================================================
--  1. FUNDAMENTAL TYPES
-- =========================================================================

type RealT = Double
type ComplexT = Complex RealT

-- Description: Abstract Syntax Tree (AST) representing a Quantum Operator.
--              Includes unitaries, states, and structural combinators.
data QOp 
  = One                   -- Neutral element (scalar 1 / 0-qubit identity)
  | Ket [Int]             -- Computational basis state |k1...kn>
  | I | X | Y | Z | H | SX -- Standard single-qubit gates
  | R QOp RealT           -- Rotation: exp(-i * theta/2 * Op)
  | C QOp                 -- Controlled-Op (Qubit 0 is Control, rest are Target)
  | Permute [Int]         -- Qubit permutation
  | Tensor QOp QOp        -- Tensor product (A ⊗ B)
  | DirectSum QOp QOp     -- Direct sum (A ⊕ B) used for classical control/choice
  | Compose QOp QOp       -- Sequential composition (B then A)
  | Adjoint QOp           -- Conjugate transpose (Inverse for unitaries)
  deriving (Show, Eq)

-- Description: Pattern synonym to treat 'Bra' as a first-class constructor.
--              Bra ks is semantically equivalent to Adjoint (Ket ks).
pattern Bra :: [Int] -> QOp
pattern Bra ks <- Adjoint (Ket ks)
  where Bra ks =  Adjoint (Ket ks)


-- =========================================================================
--  2. QUANTUM PROGRAMS
-- =========================================================================

-- Description: A single step in a quantum program.
data Step
  = Unitary QOp     -- Deterministic unitary evolution
  | Measure [Int]   -- Non-reversible measurement of specific qubits
  deriving (Show, Eq)

-- Description: A full quantum program is a sequence of steps.
type Program = [Step]


-- =========================================================================
--  3. OPERATOR TYPECLASS
-- =========================================================================

-- Description: Interface for types that behave like linear operators.
--              Allows generic manipulation (composition, tensor products) across
--              different representations (AST, Matrices, Tensor Networks).
class Semigroup o => Operator o where
  compose    :: o -> o -> o   -- Sequential composition 
  tensorProd :: o -> o -> o   -- Tensor product ⊗
  directSum  :: o -> o -> o   -- Direct sum ⊕
  adj        :: o -> o        -- Adjoint (†)

  -- Default implementation uses Semigroup for composition
  compose = (<>) 

  -- Syntactic Sugar & Unicode Aliases
  (∘), (>:), (⊗), (⊕), (<.>), (<+>) :: o -> o -> o
  
  (∘)   = compose         -- Math style: (f ∘ g) x = f(g(x))
  (>:) a b = compose b a  -- Pipe style: a >: b means a then b
  (⊗)   = tensorProd
  (⊕)   = directSum
  (<.>) = tensorProd      -- ASCII alias for Tensor
  (<+>) = directSum       -- ASCII alias for DirectSum
  

-- =========================================================================
--  4. INSTANCES
-- =========================================================================

instance Semigroup QOp where
  (<>) = Compose

instance Operator QOp where
  tensorProd = Tensor
  directSum  = DirectSum
  adj        = Adjoint

-- Operator Precedence
infixr 8 ⊗, <.>
infixr 7 ⊕, <+>
infixr 6 ∘, >: