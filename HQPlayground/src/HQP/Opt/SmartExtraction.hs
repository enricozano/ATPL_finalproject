module HQP.Opt.SmartExtraction where

import HQP.QOp.Syntax
import HQP.QOp.HelperFunctions 
import HQP.QOp.Symplectic (applyCliffordToRotation)
import HQP.Opt.GateDecomposition (toZBasis, fromZBasis, buildNaiveLadderPair)
import Data.List (partition)

-- Rimosso: import Debug.Trace (trace)

-- =========================================================================
--  1. MAIN WORKFLOW: CLIFFORD EXTRACTION OPTIMIZATION
-- =========================================================================

optimizeCircuitWithExtraction :: [QOp] -> QOp
optimizeCircuitWithExtraction rotations = 
    let 
        (physicalCircuit, extractedClifford) = processRotations rotations One
    in 
        Compose extractedClifford physicalCircuit

processRotations :: [QOp] -> QOp -> (QOp, QOp)
processRotations [] accCliff = (One, accCliff)
processRotations (currentRot : futureRots) accCliff = 
    let 
        -- 0. PRE-PROCESSING
        -- Rimosso il trace su currentRot
        (pauliString, theta) = case currentRot of
            R p t -> (p, t)
            _     -> (I, 0.0) 
        
        n = sizeOf currentRot 

        -- -----------------------------------------------------------------
        -- STEP 1: BASIS CHANGE EXTRACTION
        -- -----------------------------------------------------------------
        basisUncompute = fromZBasis pauliString 
        
        -- Rimosso debugMsg2 e basisUncompute_traced
        -- Usiamo direttamente basisUncompute
        accCliff_afterBasis = Compose accCliff basisUncompute

        -- Future' = BasisUncompute_dag * Future * BasisUncompute
        adjUncompute = Adjoint basisUncompute
        futureRots_ZFrame = updateFutures adjUncompute futureRots
        
        -- Rimosso debugMsg3 e futureRots_ZFrame_traced
        -- Usiamo direttamente futureRots_ZFrame

        -- -----------------------------------------------------------------
        -- STEP 2: SMART CNOT TREE SYNTHESIS
        -- -----------------------------------------------------------------
        activeIndices = getActiveIndices (R pauliString theta)

        -- Pass 'n' (total size) to the synthesis function
        (ladder, unladder, rootIdx) = synthesizeSmartTree n activeIndices futureRots_ZFrame

        accCliff_afterTree = Compose accCliff_afterBasis unladder

        futureRots_Final = updateFutures (Adjoint unladder) futureRots_ZFrame

        -- Rimosso debugMsg4 e futureRots_Final_traced
        -- Usiamo direttamente futureRots_Final

        -- -----------------------------------------------------------------
        -- STEP 3: EMIT PHYSICAL GATES
        -- -----------------------------------------------------------------
        basisCompute = toZBasis pauliString 
        
        -- Core Z Rotation (using correct size n)
        coreRot = R (generateZOnIndexWithSize rootIdx n) theta

        localBlock = Compose coreRot (Compose ladder basisCompute)

        (restOfCircuit, finalAcc) = processRotations futureRots_Final accCliff_afterTree
    in 
        (Compose restOfCircuit localBlock, finalAcc)


-- =========================================================================
--  2. SMART TREE SYNTHESIS (Algorithm 1)
-- =========================================================================

-- | Synthesizes a CNOT tree.
-- Now takes 'n' (total system size) to ensure gates are dimensionally correct.
synthesizeSmartTree :: Int -> [Int] -> [QOp] -> (QOp, QOp, Int)
synthesizeSmartTree n indices futures = 
    case futures of
        [] -> 
            -- Fallback: Naive ladder
            let (lad, unlad) = buildNaiveLadderPair indices n
            in (lad, unlad, if null indices then 0 else last indices)
            
        (nextRot : _) -> 
            -- FIX: Extract the Pauli string from the Rotation gate
            let nextPauli = case nextRot of
                    R p _ -> p
                    _     -> nextRot -- Fallback if it's somehow not an R gate
            in
            if length indices <= 1 then
                (One, One, if null indices then 0 else head indices)
            else
                let
                    (idxsI, idxsX, idxsY, idxsZ) = partitionIndices nextPauli indices
                    
                    -- Pass 'n' recursively
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


-- | Connects roots using the 3-step heuristic (Z->Y, I->X, Y->X).
connectRootsSmart :: Int -> [(QOp, QOp, Int)] -> QOp -> (QOp, QOp, Int)
connectRootsSmart n rootsInfo nextPauli = 
    let 
        available = [(op, idx) | (op, _, idx) <- rootsInfo]
        
        makeCNOT c t = 
            let op = genericCNOT c t n
            in (op, op)

        -- 1. Z and Y connection (Prioritize Z -> Y)
        -- Result is labeled Y (Target dominates)
        (connZY_L, connZY_U, rootsAfterZY) = 
            case (lookup Z available, lookup Y available) of
                (Just rZ, Just rY) -> 
                    let (l, u) = makeCNOT rZ rY
                        rem = filter (\(o,_) -> o /= Z && o /= Y) available
                    in (l, u, rem ++ [(Y, rY)])
                _ -> (One, One, available)

        -- 2. I and X connection (Prioritize I -> X)
        -- Result is labeled X (Target dominates)
        (connIX_L, connIX_U, rootsAfterIX) = 
            case (lookup I rootsAfterZY, lookup X rootsAfterZY) of
                (Just rI, Just rX) -> 
                    let (l, u) = makeCNOT rI rX
                        rem = filter (\(o,_) -> o /= I && o /= X) rootsAfterZY
                    in (l, u, rem ++ [(X, rX)])
                _ -> (One, One, rootsAfterZY)

        -- 3. Y and X connection (Prioritize Y -> X)  <-- NEW RULE ADDED HERE
        -- This connects the survivors of the previous two steps.
        (connYX_L, connYX_U, rootsAfterYX) = 
            case (lookup Y rootsAfterIX, lookup X rootsAfterIX) of
                (Just rY, Just rX) -> 
                    let (l, u) = makeCNOT rY rX
                        rem = filter (\(o,_) -> o /= Y && o /= X) rootsAfterIX
                    in (l, u, rem ++ [(X, rX)])
                _ -> (One, One, rootsAfterIX)

        -- 4. Linear for remaining
        finalIndices = map snd rootsAfterYX
        (linearLad, linearUnlad) = buildNaiveLadderPair finalIndices n
        finalRoot = if null finalIndices then 0 else last finalIndices

        -- Compose all connection steps in order
        -- Order of Application (Inner to Outer / Right to Left):
        -- Linear . YX . IX . ZY
        totalLadder = Compose linearLad (Compose connYX_L (Compose connIX_L connZY_L))
        totalUnlad  = Compose connZY_U (Compose connIX_U (Compose connYX_U linearUnlad))

    in (totalLadder, totalUnlad, finalRoot)


-- =========================================================================
--  3. HELPER FUNCTIONS
-- =========================================================================

updateFutures :: QOp -> [QOp] -> [QOp]
updateFutures clifford ops = 
    map (applyCliffordToRotation clifford) ops

partitionIndices :: QOp -> [Int] -> ([Int], [Int], [Int], [Int])
partitionIndices pauli indices = 
    let 
        classify idx = getPauliOpAt pauli idx
        (is, rest1) = partition (\i -> classify i == I) indices
        (xs, rest2) = partition (\i -> classify i == X) rest1
        (ys, zs)    = partition (\i -> classify i == Y) rest2
    in 
        (is, xs, ys, zs)

getPauliOpAt :: QOp -> Int -> QOp
getPauliOpAt op idx = 
    let ops = flattenOps op 
    in if idx >= 0 && idx < length ops then ops !! idx else I 

getActiveIndices :: QOp -> [Int]
getActiveIndices (R pauli _) = 
    [i | (i, p) <- zip [0..] (flattenOps pauli), p /= I]
getActiveIndices _ = []

getAngle :: QOp -> RealT
getAngle (R _ theta) = theta
getAngle _ = 0.0

generateZOnIndexWithSize :: Int -> Int -> QOp
generateZOnIndexWithSize idx n = 
    let pre  = nId idx
        post = nId (n - idx - 1)
    in Tensor pre (Tensor Z post)