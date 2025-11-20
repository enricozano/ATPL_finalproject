module Main where 

import QubitOperators
import MatrixSemantics
import PrettyMatrix
import PrettyOp
import System.Random(mkStdGen, randoms)
-- import Data.List(intercalate)

teleprog :: Program
teleprog = let
        op1 = Unitary $ 
            I ⊗ I ⊗ I -- Replace by your own program
        
        op2 = Unitary $ 
            I ⊗ I ⊗ I -- Replace by your own program
    in
        [op1, Measure [0,1], op2]

teleport :: RNG -> StateT -> (StateT, RNG)
teleport rng psi = let
	-- Write this yourself
    in
        evalProg rng teleprog psi_bell


main :: IO()
main = do
    let rng0 = randoms (mkStdGen 42) :: [Double]    
    --let rng0 = [0,0,0] -- (always measure 0)
    -- let rng0 = [0,1,0] -- (First meausure 0, then measure 1)
    
    let psi = sqrt(1/3) .* ket [0] + sqrt(2/3) .* ket [1]
        bell     = sqrt(1/2)*(ket [0,0] + ket [1,1])
        psi_bell = psi `tensorProd` bell

    putStr $ "\nTeleport program:\n" ++ showProgram teleprog ++ "\n\n"

--    let path = [False, True,False]
--    let steps = [ fst $ evalStepOp 3 path step| step <- teleprog ]
--    putStr $ "Teleport program matrix semantics:\n" ++ (intercalate "\n" (map showCMat steps)) ++ "\n\n"

    putStr $ "|ψ>   = "++(showState psi) ++ "\n" -- ". Running teleport program!\n"
    putStr $ "|ψbb> = "++(showState psi_bell)++".\nRunning teleport program!\n"

    let (end_state,_) = teleport rng0 psi

    putStr $ "Final 3-qubit state:\n" ++ (showState end_state) ++ "\n\n"

    

    
