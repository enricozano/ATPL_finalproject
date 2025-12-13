module PI where

import HQP hiding (fixpoint)
import Programs.QFT
import qualified HQP.QOp.MatrixSemantics as MatSem 
import Data.List (sort)

-- Utility for pretty printing
pp :: QOp -> IO ()
pp = putStrLn . showOp 

-- ==========================================
-- 1. HELPER FUNCTIONS
-- ==========================================

-- | Generates the Identity operator on N qubits.
--   Used to pad circuits (e.g., when a CNOT is applied on a subset of wires).
nId :: Int -> QOp
nId 0 = One
nId 1 = I
nId n = Tensor I (nId (n - 1))

-- | Calculates the number of qubits required for a given Operator.
--   Traverses the AST to find the total width.
sizeOf :: QOp -> Int
sizeOf One = 0
sizeOf I = 1
sizeOf X = 1
sizeOf Y = 1
sizeOf Z = 1
sizeOf H = 1
sizeOf SX = 1
sizeOf (C op) = 1 + sizeOf op        -- Control adds 1 qubit
sizeOf (R op _) = sizeOf op          -- Rotation preserves size
sizeOf (Tensor a b) = sizeOf a + sizeOf b
sizeOf (Compose a _) = sizeOf a      -- Compose assumes dimensions match
sizeOf _ = 1                         -- Fallback assumption

-- | Creates a CNOT gate between qubit 'i' and 'i+1' in a system of 'n' qubits.
adjacentCNOT :: Int -> Int -> QOp
adjacentCNOT i n 
    | i >= n - 1 = One -- Error: Index out of bounds
    | i == 0     = Tensor (C X) (nId (n - 2))
    | otherwise  = Tensor I (adjacentCNOT (i - 1) (n - 1))

-- ==========================================
-- 2. PAULI GADGET COMPONENTS
-- ==========================================

-- | Pre-processing: Changes the basis of each qubit to Z.
--   X -> Z (via H)
--   Y -> Z (via S_dag H)
--   Z -> Z (Identity)
toZBasis :: QOp -> QOp
toZBasis X = H
toZBasis Y = R X (pi/2) -- Equivalent to S_dag H (rotates Y to Z)
toZBasis Z = I          
toZBasis I = I
toZBasis (Tensor a b) = Tensor (toZBasis a) (toZBasis b) 
toZBasis op = op        -- Fallback

-- | Post-processing: Restores the original basis (Inverse of toZBasis).
fromZBasis :: QOp -> QOp
fromZBasis X = H        
fromZBasis Y = R X (-pi/2) -- Inverse of (R X pi/2)
fromZBasis Z = I
fromZBasis I = I
fromZBasis (Tensor a b) = Tensor (fromZBasis a) (fromZBasis b)
fromZBasis op = op

-- | Constructs the "Ladder" of CNOTs to compute parity on the last qubit.
buildLadder :: Int -> QOp
buildLadder n 
    | n < 2 = One 
    | otherwise = foldl1 Compose [adjacentCNOT i n | i <- [0 .. n - 2]]

-- | Applies the core rotation (Rz) on the last qubit of the chain.
lastQubitRot :: Int -> RealT -> QOp
lastQubitRot n theta = Tensor (nId (n - 1)) (R Z theta)

-- ==========================================
-- 3. EXPANSION LOGIC (The Compiler Pass)
-- ==========================================

-- | Expands a single high-level Pauli Rotation (R P theta) into 
--   hardware-primitive Clifford+Rotation gates.
expandPauliRot :: QOp -> QOp

-- OPTIMIZATION 1: Base Case Z
-- If it's already a Z rotation, no compilation needed.
expandPauliRot (R Z theta) = R Z theta 

-- OPTIMIZATION 2: Base Case X
-- Direct decomposition: Rx(theta) = H * Rz(theta) * H
expandPauliRot (R X theta) = 
    Compose H (Compose (R Z theta) H)

-- OPTIMIZATION 3: Base Case Y
-- Direct decomposition: Ry(theta) = S_dag * H * Rz(theta) * H * S
expandPauliRot (R Y theta) = 
    let 
        sGate = R Z (pi/2) 
        sDag  = Adjoint sGate
    in 
        Compose sDag 
          (Compose H 
            (Compose (R Z theta) 
              (Compose H sGate)))

-- GENERAL CASE: Multi-qubit Pauli Strings (The Gadget)
expandPauliRot (R pauliString theta) =
    let 
        n = sizeOf pauliString
        
        preOps  = toZBasis pauliString
        postOps = fromZBasis pauliString
        
        ladder   = buildLadder n
        
        -- The reverse ladder (uncompute)
        unladder 
            | n < 2 = One
            | otherwise = foldr1 Compose [adjacentCNOT i n | i <- reverse [0 .. n - 2]]
            
        core = lastQubitRot n theta
        
    in
        if n == 0 then One else
        Compose preOps 
          (Compose ladder 
            (Compose core 
              (Compose unladder postOps)))

-- Pass-through for non-rotation operators
expandPauliRot op = op

-- | Recursive traversal: Applies expansion to the entire circuit AST.
expandAllPauliGadgets :: QOp -> QOp
expandAllPauliGadgets op = case op of
    R pauli theta -> expandPauliRot (R pauli theta)
    Tensor a b    -> Tensor (expandAllPauliGadgets a) (expandAllPauliGadgets b)
    Compose a b   -> Compose (expandAllPauliGadgets a) (expandAllPauliGadgets b)
    DirectSum a b -> DirectSum (expandAllPauliGadgets a) (expandAllPauliGadgets b)
    C x           -> C (expandAllPauliGadgets x)
    Adjoint x     -> Adjoint (expandAllPauliGadgets x)
    _             -> op

-- ==========================================
-- 4. WHATS BELOW THIS IS BEEN DONE BY CHAT GPT HENCE WE HAVE TO CHECK AND MODIFY IT
-- ==========================================

-- | Helper: Check if an operator is I ⊗ I)
isIdentity :: QOp -> Bool
isIdentity One = True
isIdentity I = True
isIdentity (Tensor a b) = isIdentity a && isIdentity b
isIdentity _ = False

-- | Helper: Identifica i gate che sono inversi di se stessi
isSelfInverse :: QOp -> Bool
isSelfInverse H = True
isSelfInverse X = True
isSelfInverse Y = True
isSelfInverse Z = True
isSelfInverse (C _) = True 
isSelfInverse (Tensor a b) = isSelfInverse a && isSelfInverse b
isSelfInverse _ = False

-- | Optimization Pass: Peephole optimization
--   Rimuove gate adiacenti identici (se self-inverse)
optimizePass :: [QOp] -> [QOp]
optimizePass [] = []
optimizePass (x:y:xs) 
    | x == y && isSelfInverse x = optimizePass xs -- Cancellazione! Salta entrambi
    | otherwise                 = x : optimizePass (y:xs)
optimizePass (x:xs) = x : optimizePass xs

-- | Loop di Ottimizzazione (Fixpoint)
--   Continua a pulire e ottimizzare finché il circuito non cambia più
optimizeUntilStable :: [QOp] -> [QOp]
optimizeUntilStable ops =
    let 
        -- 1. Rimuovi tutte le identità (i "muri invisibili")
        cleanOps = filter (not . isIdentity) ops
        
        -- 2. Prova a cancellare i gate adiacenti
        newOps = optimizePass cleanOps
    in 
        -- 3. Se è cambiato qualcosa, ricomincia da capo!
        if newOps == ops 
        then ops 
        else optimizeUntilStable newOps

-- | Main optimization function
optimizeCircuit :: QOp -> QOp
optimizeCircuit op = 
    let 
        expanded = expandAllPauliGadgets op
        flat = flattenCompose expanded
        optimized = optimizeUntilStable flat -- Usa la versione ricorsiva
    in 
        unflattenCompose optimized

-- Helper per appiattire/ricostruire (uguali a prima)
flattenCompose :: QOp -> [QOp]
flattenCompose (Compose a b) = flattenCompose a ++ flattenCompose b
flattenCompose One = []
flattenCompose op = [op]

unflattenCompose :: [QOp] -> QOp
unflattenCompose [] = One
unflattenCompose [x] = x
unflattenCompose xs = foldr1 Compose xs

-- 1. Definiamo l'operatore di interazione (Hamiltoniana Ising)
interactionZZ :: QOp
interactionZZ = Tensor Z Z

-- 2. Creiamo un circuito che simula due step temporali successivi
--    Step 1: Angolo 0.3
--    Step 2: Angolo 0.4
trotterCircuit :: QOp
trotterCircuit = Compose (R interactionZZ 0.3) (R interactionZZ 0.4)

-- 3. Funzione di test per mostrare il Prima e Dopo
runOptimizationTest :: IO ()
runOptimizationTest = do
    putStrLn "=== CIRCUITO ORIGINALE (High Level) ==="
    pp trotterCircuit
    
    putStrLn "\n=== ESPANSIONE NAIVE (Low Level) ==="
    let expanded = expandAllPauliGadgets trotterCircuit
    pp expanded
    putStrLn $ "Lunghezza (approx): " ++ show (length $ flattenCompose expanded)
    
    putStrLn "\n=== CIRCUITO OTTIMIZZATO (Peephole) ==="
    let optimized = optimizeCircuit trotterCircuit
    pp optimized
    putStrLn $ "Lunghezza (approx): " ++ show (length $ flattenCompose optimized)
