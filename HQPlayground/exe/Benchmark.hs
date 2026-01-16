module Main where

import Text.Printf
import System.Random (newStdGen, randomRs)
import Control.Monad (replicateM_)
import System.CPUTime (getCPUTime)

-- MODULE IMPORTS
import HQP.QOp.Syntax
import HQP.Opt.GateDecomposition (optimizeCircuit)
import HQP.Opt.SmartExtraction (optimizeCircuitWithExtraction, countCNOTs)

-- =========================================================================
--  1. HELPER UTILITIES
-- =========================================================================

randomPauli :: Int -> [Int] -> QOp
randomPauli size ints = 
    let mapIntToPauli 0 = I
        mapIntToPauli 1 = X
        mapIntToPauli 2 = Y
        mapIntToPauli 3 = Z
        mapIntToPauli _ = I
        
        paulis = map mapIntToPauli (take size ints)
        
        tensorAll [] = One
        tensorAll [x] = x
        tensorAll (x:xs) = Tensor x (tensorAll xs)
    in tensorAll paulis

generateRandomCircuit :: Int -> Int -> IO [QOp]
generateRandomCircuit nQubits depth = do
    g <- newStdGen
    let randomInts = randomRs (0, 3) g :: [Int]
    let randomDoubles = randomRs (0.0, 2 * pi) g :: [Double]
    
    let makeLayer i = 
            let pString = randomPauli nQubits (drop (i*nQubits) randomInts)
                angle   = randomDoubles !! i
            in R pString angle

    return $ map makeLayer [0..depth-1]

-- Helper to measure execution time and CNOT count simultaneously
-- Since Haskell is lazy, the optimization only happens when we force the result (via count).
measureOptimization :: QOp -> IO (Double, Int)
measureOptimization op = do
    start <- getCPUTime
    let count = countCNOTs op
    end <- count `seq` getCPUTime
    let diff = (fromIntegral (end - start)) / 1e12
    return (diff, count)

-- =========================================================================
--  2. BATCH BENCHMARK RUNNER
-- =========================================================================

runBatchBenchmark :: Int -> Int -> Int -> IO ()
runBatchBenchmark n d iterations = do
    -- Run the loop 'iterations' times
    replicateM_ iterations $ do
        -- 1. Generate Circuit
        circuitOps <- generateRandomCircuit n d
        let rawCircuit = foldl Compose One circuitOps
        
        -- 2. Naive Optimization (Time & Count)
        (timeNaive, cnotNaive) <- measureOptimization (optimizeCircuit rawCircuit)
        
        -- 3. Smart (QuCLEAR) Optimization (Time & Count)
        (timeSmart, cnotSmart) <- measureOptimization (optimizeCircuitWithExtraction circuitOps)
        
        -- 4. Calculate Reduction Stats
        let reduction = if cnotNaive == 0 
                        then 0.0 
                        else 100.0 * (fromIntegral (cnotNaive - cnotSmart) / fromIntegral cnotNaive) :: Double

        -- 5. Print CSV Row: Qubits, Depth, NaiveCNOTs, SmartCNOTs, ReductionPct, NaiveTime, SmartTime
        printf "%d,%d,%d,%d,%.4f,%.6f,%.6f\n" n d cnotNaive cnotSmart reduction timeNaive timeSmart

-- =========================================================================
--  MAIN
-- =========================================================================

main :: IO ()
main = do
    -- Print CSV Header (Now includes Time columns)
    putStrLn "Qubits,Depth,NaiveCNOTs,SmartCNOTs,ReductionPct,NaiveTime,SmartTime"
    
    -- Run Case A: 20 Qubits, Depth 20, 500 iterations
    runBatchBenchmark 20 20 500
    
    -- Run Case B: 50 Qubits, Depth 50, 500 iterations
    runBatchBenchmark 50 50 100

    --could add other cases with more qubits/depth if needed