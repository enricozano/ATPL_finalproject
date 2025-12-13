module HQP.QOp.Optimiser where 
import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions
import Data.Function (fix)

-- Gate da applicare PRIMA della scala di CNOT
toZBasis :: QOp -> QOp
toZBasis X = H
toZBasis Y = R X (pi/2) -- Ruota Y sull'asse Z
toZBasis Z = I          -- Già in Z, facciamo l'identità
toZBasis I = I
toZBasis (Tensor a b) = Tensor (toZBasis a) (toZBasis b) -- Ricorsione

-- Gate da applicare DOPO (l'inverso)
fromZBasis :: QOp -> QOp
fromZBasis X = H        -- H è l'inverso di se stesso
fromZBasis Y = R X (-pi/2) 
fromZBasis Z = I
fromZBasis I = I
fromZBasis (Tensor a b) = Tensor (fromZBasis a) (fromZBasis b) -- Ricorsione

expandPauliRot :: QOp -> RealT -> QOp
expandPauliRot pauliString theta = 
  let
      -- 1. Prepara i qubit (tutti in base Z)
      preOps  = toZBasis pauliString
      
      -- 2. Calcola parità (CNOT a catena verso l'ultimo qubit)
      -- Nota: qui dovresti generare la scala di CNOT in base alla dimensione di pauliString
      ladder  = generateCNOTLadder (countQubits pauliString)
      
      -- 3. La rotazione vera e propria (RZ sull'ultimo qubit)
      -- Assumiamo che 'lastQubitRZ' applichi I su tutti tranne RZ sull'ultimo
      coreRot = R Z theta -- Applicato solo all'ultimo qubit della catena
      
      -- 4. Smonta la scala (CNOT al contrario)
      unladder = reverseLadder ladder
      
      -- 5. Ripristina le basi originali
      postOps = fromZBasis pauliString
      
  in
      -- Sequenza: Pre -> CNOTs -> Rotazione -> CNOTs inv -> Post
      Compose preOps 
        (Compose ladder 
          (Compose coreRot 
            (Compose unladder postOps)))