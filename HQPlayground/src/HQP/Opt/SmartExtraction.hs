module HQP.Opt.SmartExtraction where

import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions 
import HQP.QOp.Symplectic (applyCliffordToRotation)
import HQP.Opt.GateDecomposition (toZBasis, fromZBasis, buildNaiveLadderPair)
import Data.List (partition)

-- Description: Main entry point to optimize a sequence of rotations by extracting Clifford gates and optimizing parity networks.
-- Inputs: A list of Quantum Operators (rotations).
-- Outputs: A single composed Quantum Operator representing the optimized circuit.
optimizeCircuitWithExtraction :: [QOp] -> QOp
optimizeCircuitWithExtraction rotations = 
    let 
        (physicalCircuit, extractedClifford) = processRotations rotations One
    in 
        Compose extractedClifford physicalCircuit

-- Description: Recursively processes rotations to perform basis changes, synthesize smart CNOT trees, and push Cliffords forward.
-- Inputs: A list of future rotations to process, and the currently accumulated Clifford operator.
-- Outputs: A tuple containing the optimized physical circuit block and the final accumulated Clifford operator.
processRotations :: [QOp] -> QOp -> (QOp, QOp)
processRotations [] accCliff = (One, accCliff)
processRotations (currentRot : futureRots) accCliff = 
    let 
        (pauliString, theta) = case currentRot of
            R p t -> (p, t)
            _     -> (I, 0.0) 
        
        n = sizeOf currentRot 

        basisUncompute = fromZBasis pauliString 
        
        accCliff_afterBasis = Compose accCliff basisUncompute

        adjUncompute = Adjoint basisUncompute
        futureRots_ZFrame = updateFutures adjUncompute futureRots
        
        activeIndices = getActiveIndices (R pauliString theta)

        (ladder, unladder, rootIdx) = synthesizeSmartTree n activeIndices futureRots_ZFrame

        accCliff_afterTree = Compose accCliff_afterBasis unladder

        futureRots_Final = updateFutures (Adjoint unladder) futureRots_ZFrame

        basisCompute = toZBasis pauliString 
        
        coreRot = R (generateZOnIndexWithSize rootIdx n) theta

        localBlock = Compose coreRot (Compose ladder basisCompute)

        (restOfCircuit, finalAcc) = processRotations futureRots_Final accCliff_afterTree
    in 
        (Compose restOfCircuit localBlock, finalAcc)


-- Description: Synthesizes a CNOT ladder (tree) optimized based on the structure of future Pauli strings.
-- Inputs: The total number of qubits, a list of active indices, and a list of future rotation operators.
-- Outputs: A tuple containing the ladder operator, the un-ladder operator, and the index of the root qubit.
synthesizeSmartTree :: Int -> [Int] -> [QOp] -> (QOp, QOp, Int)
synthesizeSmartTree n indices futures = 
    case futures of
        [] -> 
            let (lad, unlad) = buildNaiveLadderPair indices n
            in (lad, unlad, if null indices then 0 else last indices)
            
        (nextRot : _) -> 
            let nextPauli = case nextRot of
                    R p _ -> p
                    _     -> nextRot
            in
            if length indices <= 1 then
                (One, One, if null indices then 0 else head indices)
            else
                let
                    (idxsI, idxsX, idxsY, idxsZ) = partitionIndices nextPauli indices
                    
                    recCall idxs = synthesizeSmartTree n idxs (tail futures)
                    
                    (ladI, unladI, rI) = if null idxsI then (One, One, -1) else recCall idxsI
                    (ladX, unladX, rX) = if null idxsX then (One, One, -1) else recCall idxsX
                    (ladY, unladY, rY) = if null idxsY then (One, One, -1) else recCall idxsY
                    (ladZ, unladZ, rZ) = if null idxsZ then (One, One, -1) else recCall idxsZ
                    
                    validRoots = filter (\(_, _, r) -> r /= -1) 
                                 [(I, ladI, rI), (X, ladX, rX), (Y, ladY, rY), (Z, ladZ, rZ)]
                    
                    (rootLad, rootUnlad, finalRoot) = connectRootsSmart n validRoots nextPauli
                    
                    subLadders   = foldl Compose One [l | (_, l, _) <- validRoots]
                    totalLadder  = Compose rootLad subLadders
                    
                    subUnladders = foldl Compose One [if opType == I then unladI 
                                                      else if opType == X then unladX
                                                      else if opType == Y then unladY
                                                      else unladZ 
                                                      | (opType, _, _) <- validRoots]
                    totalUnladder = Compose subUnladders rootUnlad
                in
                    (totalLadder, totalUnladder, finalRoot)


-- Description: Connects the roots of different Pauli sub-trees using heuristic rules (Z->Y, I->X, Y->X) to optimize the ladder structure.
-- Inputs: Total number of qubits, a list of root information tuples (Pauli type, Ladder op, Root Index), and the next Pauli operator (unused).
-- Outputs: A tuple containing the connecting ladder, connecting un-ladder, and the final root index.
connectRootsSmart :: Int -> [(QOp, QOp, Int)] -> QOp -> (QOp, QOp, Int)
connectRootsSmart n rootsInfo _ = 
    let 
        available = [(op, idx) | (op, _, idx) <- rootsInfo]
        
        makeCNOT c t = 
            let op = genericCNOT c t n
            in (op, op)

        (connZY_L, connZY_U, rootsAfterZY) = 
            case (lookup Z available, lookup Y available) of
                (Just rZ, Just rY) -> 
                    let (l, u) = makeCNOT rZ rY
                        remaining = filter (\(o,_) -> o /= Z && o /= Y) available
                    in (l, u, remaining ++ [(Y, rY)])
                _ -> (One, One, available)

        (connIX_L, connIX_U, rootsAfterIX) = 
            case (lookup I rootsAfterZY, lookup X rootsAfterZY) of
                (Just rI, Just rX) -> 
                    let (l, u) = makeCNOT rI rX
                        remaining = filter (\(o,_) -> o /= I && o /= X) rootsAfterZY
                    in (l, u, remaining ++ [(X, rX)])
                _ -> (One, One, rootsAfterZY)

        (connYX_L, connYX_U, rootsAfterYX) = 
            case (lookup Y rootsAfterIX, lookup X rootsAfterIX) of
                (Just rY, Just rX) -> 
                    let (l, u) = makeCNOT rY rX
                        remaining = filter (\(o,_) -> o /= Y && o /= X) rootsAfterIX
                    in (l, u, remaining ++ [(X, rX)])
                _ -> (One, One, rootsAfterIX)

        finalIndices = map snd rootsAfterYX
        (linearLad, linearUnlad) = buildNaiveLadderPair finalIndices n
        finalRoot = if null finalIndices then 0 else last finalIndices

        totalLadder = Compose linearLad (Compose connYX_L (Compose connIX_L connZY_L))
        totalUnlad  = Compose connZY_U (Compose connIX_U (Compose connYX_U linearUnlad))

    in (totalLadder, totalUnlad, finalRoot)


-- Description: Updates a list of future rotation operators by applying a Clifford transformation to them.
-- Inputs: A Clifford operator and a list of rotation operators.
-- Outputs: A list of transformed rotation operators.
updateFutures :: QOp -> [QOp] -> [QOp]
updateFutures clifford ops = 
    map (applyCliffordToRotation clifford) ops

-- Description: Partitions a list of qubit indices into four lists (I, X, Y, Z) corresponding to the Pauli operator acting on them.
-- Inputs: A Pauli operator and a list of qubit indices to classify.
-- Outputs: A 4-tuple of integer lists corresponding to indices for I, X, Y, and Z.
partitionIndices :: QOp -> [Int] -> ([Int], [Int], [Int], [Int])
partitionIndices pauli indices = 
    let 
        classify idx = getPauliOpAt pauli idx
        (is, rest1) = partition (\i -> classify i == I) indices
        (xs, rest2) = partition (\i -> classify i == X) rest1
        (ys, zs)    = partition (\i -> classify i == Y) rest2
    in 
        (is, xs, ys, zs)

-- Description: Retrieves the single-qubit Pauli operator acting on a specific index within a larger Pauli string.
-- Inputs: A Pauli operator and the target qubit index.
-- Outputs: The single-qubit Pauli operator (I, X, Y, or Z) at that index.
getPauliOpAt :: QOp -> Int -> QOp
getPauliOpAt op idx = 
    let ops = flattenOps op 
    in if idx >= 0 && idx < length ops then ops !! idx else I 


-- Description: Extracts the rotation angle theta from a Rotation operator.
-- Inputs: A Rotation operator (R).
-- Outputs: The rotation angle as a RealT (Double).
getAngle :: QOp -> RealT
getAngle (R _ theta) = theta
getAngle _ = 0.0

-- Description: Constructs a Z rotation operator on a specific qubit index within an n-qubit system.
-- Inputs: The target qubit index and the total number of qubits.
-- Outputs: A Tensor product operator representing Z on the target index and Identity elsewhere.
generateZOnIndexWithSize :: Int -> Int -> QOp
generateZOnIndexWithSize idx n = 
    let pre  = nId idx
        post = nId (n - idx - 1)
    in Tensor pre (Tensor Z post)

-- Description: Counts the total number of CNOT gates in a given quantum operator tree.
-- Inputs: A Quantum Operator.
-- Outputs: The integer count of CNOT gates.
countCNOTs :: QOp -> Int
countCNOTs op = case op of
    Compose a b   -> countCNOTs a + countCNOTs b
    Tensor a b    -> countCNOTs a + countCNOTs b
    DirectSum a b -> countCNOTs a + countCNOTs b
    Adjoint a     -> countCNOTs a  
    C innerOp     -> countControlledX innerOp
    _             -> 0

-- Description: Auxiliary function to count X gates within a Controlled operator structure, effectively counting CNOTs.
-- Inputs: The inner operator of a Controlled gate.
-- Outputs: The integer count of X gates found.
countControlledX :: QOp -> Int
countControlledX op = case op of
    X           -> 1
    Tensor a b  -> countControlledX a + countControlledX b
    Compose a b -> countControlledX a + countControlledX b
    Adjoint a   -> countControlledX a 
    _           -> 0