module HQP.QOp.Optimiser where 
import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions
import Data.Function (fix)

-- Gates to apply BEFORE the CNOT ladder
toZBasis :: QOp -> QOp
toZBasis X = H
toZBasis Y = R X (pi/2) -- Rotate Y around X-axis to align with Z
toZBasis Z = I          -- Z gate only need to apply Identity
toZBasis I = I
toZBasis (Tensor a b) = Tensor (toZBasis a) (toZBasis b) -- Recursion

-- Gates to apply AFTER (the inverse)
fromZBasis :: QOp -> QOp
fromZBasis X = H        -- Hadamard inverse
fromZBasis Y = R X (-pi/2)  -- Inverse rotation
fromZBasis Z = I
fromZBasis I = I
fromZBasis (Tensor a b) = Tensor (fromZBasis a) (fromZBasis b) -- Recursion

expandPauliRot :: QOp -> RealT -> QOp
expandPauliRot pauliString theta = 
  let
      -- 1. Prepare qubits (all in Z-basis)
      preOps  = toZBasis pauliString
      
      -- 2. Compute parity (CNOT chain towards the last qubit)
      -- Note: here you should generate the CNOT ladder based on the size of pauliString
      ladder  = generateCNOTLadder (countQubits pauliString)
      
      -- 3. The actual rotation (RZ on the last qubit)
      -- We assume that 'lastQubitRZ' applies I on all except RZ on the last one
      coreRot = R Z theta -- Applied only to the last qubit of the chain
      
      -- 4. Dismantle the ladder (CNOT in reverse)
      unladder = reverseLadder ladder
      
      -- 5. Restore original bases
      postOps = fromZBasis pauliString
      
  in
      -- Sequence: Pre -> CNOTs -> Rotation -> Inverse CNOTs -> Post
      Compose preOps 
        (Compose ladder 
          (Compose coreRot 
            (Compose unladder postOps)))