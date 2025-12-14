module PI where

import HQP hiding (fixpoint)
import Programs.QFT
import qualified HQP.QOp.MatrixSemantics as MatSem 
import Data.List (sort)
import Data.Maybe (fromMaybe)

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
-- 4. CLIFFORD EXTRACTION LOGIC
-- ==========================================

{- 
   Questa sezione implementa la logica per spostare tutti i gate di Clifford
   alla "sinistra" della composizione (ovvero alla fine temporale del circuito),
   lasciando le rotazioni Pauli "a destra" (inizio temporale).
   
   Matematicamente: Rot . Cliff -> Cliff . Rot'
   Dove Rot' è la rotazione coniugata da Cliff.
-}

-- | Tipo per rappresentare una stringa di Pauli con un segno (+/- 1)
--   Il segno è importante perché Cliffords possono introdurre fasi negative.
--   Es: X . Y . X = -Y
type SignedPauli = (QOp, RealT)

-- | Verifica se un operatore è un gate di Clifford "puro".
isClifford :: QOp -> Bool
isClifford op = case op of
    I -> True
    X -> True
    Y -> True
    Z -> True
    H -> True
    SX -> True -- Sqrt X è Clifford
    C X -> True -- CNOT è Clifford
    Permute _ -> True
    Tensor a b -> isClifford a && isClifford b
    Compose a b -> isClifford a && isClifford b
    Adjoint a -> isClifford a
    DirectSum a b -> isClifford a && isClifford b
    One -> True
    R _ _ -> False -- Consideriamo tutte le R come non-Clifford per ora
    _ -> False     -- Fallback per sicurezza

-- | Calcola l'inverso (aggiunto) di un gate di Clifford.
--   Nota: Per H, X, Y, Z, CNOT l'inverso è se stesso.
--   Per S (o SX) e composizioni, bisogna fare attenzione.
invClifford :: QOp -> QOp
invClifford op = case op of
    I -> I
    X -> X
    Y -> Y
    Z -> Z
    H -> H
    SX -> R X (-pi/2) -- SX inverse è rotazione opposta (o SX_dag)
    C a -> C (invClifford a)
    Tensor a b -> Tensor (invClifford a) (invClifford b)
    Compose a b -> Compose (invClifford b) (invClifford a) -- (AB)^+ = B^+ A^+
    Adjoint a -> a
    Permute ks -> Adjoint (Permute ks) -- Permute inverso
    One -> One
    _ -> Adjoint op -- Fallback generico

-- | Il cuore dell'algoritmo: Coniuga una Pauli P con un Clifford C.
--   Restituisce C' P C e il segno risultante.
--   Implementa le regole di commutazione standard.
conjugate :: QOp -> QOp -> SignedPauli
conjugate cliff pauli = case (cliff, pauli) of
    -- CASO MANCANTE: Gestione Catene di Clifford (Compose A B)
    -- In Haskell 'Compose outer inner' applica 'inner' poi 'outer'.
    -- Quindi coniughiamo prima con 'inner', e il risultato con 'outer'.
    (Compose outer inner, p) -> 
        let (p', s1)  = conjugate inner p      -- Prima attraversa inner
            (p'', s2) = conjugate outer p'     -- Poi attraversa outer
        in (p'', s1 * s2)

    -- Identità
    (I, p) -> (p, 1.0)
    (One, p) -> (p, 1.0)
    (_, I) -> (I, 1.0)
    (_, One) -> (One, 1.0)

    -- Stesso operatore (Auto-coniugazione)
    (X, X) -> (X, 1.0)
    (Y, Y) -> (Y, 1.0)
    (Z, Z) -> (Z, 1.0)
    (H, H) -> (H, 1.0) 

    -- Regole H
    (H, X) -> (Z, 1.0)
    (H, Z) -> (X, 1.0)
    (H, Y) -> (Y, -1.0) 
    
    -- Regole X
    (X, Y) -> (Y, -1.0) 
    (X, Z) -> (Z, -1.0)
    
    -- Regole Y
    (Y, X) -> (X, -1.0) 
    (Y, Z) -> (Z, -1.0) 
    
    -- Regole Z
    (Z, X) -> (X, -1.0)
    (Z, Y) -> (Y, -1.0)
    
    -- CNOT (ricorsione gestita da propagateCNOT)
    (C X, Tensor pC pT) -> propagateCNOT pC pT
    
    -- Tensori
    (Tensor c1 c2, Tensor p1 p2) -> 
        let (p1', s1) = conjugate c1 p1
            (p2', s2) = conjugate c2 p2
        in (Tensor p1' p2', s1 * s2)
    
    -- Fallback intelligente per Tensori misti
    (Tensor c1 c2, p) -> case p of
        Tensor p1 p2 -> conjugate (Tensor c1 c2) (Tensor p1 p2)
        I -> (I, 1.0) 
        _ -> (p, 1.0) -- Caso estremo (es. dimensioni non corrispondenti)

    -- Caso default
    (_, p) -> (p, 1.0)

-- | Logica di propagazione specifica per CNOT (Controllo X, Target Identity)
--   Regole:
--   XI -> XX, ZI -> ZI, IX -> IX, IZ -> ZZ
--   YI -> YX, IY -> ZY, etc.
propagateCNOT :: QOp -> QOp -> SignedPauli
propagateCNOT pC pT = case (pC, pT) of
    (I, I) -> (Tensor I I, 1)
    (X, I) -> (Tensor X X, 1) -- X propaga avanti
    (Z, I) -> (Tensor Z I, 1)
    (I, X) -> (Tensor I X, 1)
    (I, Z) -> (Tensor Z Z, 1) -- Z propaga indietro
    (X, X) -> (Tensor X I, 1) -- XX -> XI
    (Z, Z) -> (Tensor I Z, 1) -- ZZ -> IZ
    (Y, I) -> (Tensor Y X, 1) -- Y = iXZ. (X->XX)(Z->Z) = XXZ = -Y X
    (I, Y) -> (Tensor Z Y, 1) 
    -- ... altri casi combinatori (YY, etc.)
    -- Implementazione semplificata: delega alla composizione se complessa
    _ -> (Tensor pC pT, 1) -- Placeholder per brevità

-- | Applica la coniugazione a un operatore di Rotazione.
--   Se R(P, theta), calcola C' P C = s * P' e restituisce R(P', s * theta)
applyCliffordToRotation :: QOp -> QOp -> QOp
applyCliffordToRotation cliff op = case op of
    R pauli theta -> 
        let (newPauli, sign) = conjugate cliff pauli
        in R newPauli (theta * sign)
    Compose a b -> Compose (applyCliffordToRotation cliff a) (applyCliffordToRotation cliff b)
    _ -> op -- Non dovrebbe accadere se separiamo correttamente

-- | Funzione Principale: Spinge tutti i Cliffords alla fine (sinistra nella composizione).
--   Restituisce (Parte_Rotazioni, Parte_Cliffords)
pushCliffordsToEnd :: QOp -> (QOp, QOp)
pushCliffordsToEnd op 
    | isClifford op = (nId (sizeOf op), op) -- È tutto Clifford
    | otherwise = case op of
        R pauli theta -> (R pauli theta, nId (sizeOf pauli)) -- È solo Rotazione
        
        Compose a b -> -- a viene dopo b (Matematicamente a o b)
            let (rotA, cliffA) = pushCliffordsToEnd a
                (rotB, cliffB) = pushCliffordsToEnd b
                
                -- Situazione attuale: cliffA . rotA . cliffB . rotB
                -- Vogliamo: (cliffA . cliffB) . (rotA' . rotB)
                -- Dobbiamo spostare cliffB a sinistra attraverso rotA.
                -- rotA diventa rotA trasformata dall'INVERSO di cliffB
                -- (perché stiamo muovendo cliffB da "prima" a "dopo" nel tempo relativo alla rotazione)
                
                cliffB_inv = invClifford cliffB
                rotA_trans = applyCliffordToRotation cliffB_inv rotA
                
            in (Compose rotA_trans rotB, Compose cliffA cliffB)

        Tensor a b -> 
            let (ra, ca) = pushCliffordsToEnd a
                (rb, cb) = pushCliffordsToEnd b
            in (Tensor ra rb, Tensor ca cb)
            
        _ -> (op, nId (sizeOf op)) -- Fallback
-- ==========================================
-- 5. NORMALIZZAZIONE AST (Fix per Tensor)
-- ==========================================

-- | Trasforma strutture "Tensor (R ...) I" in "R (Tensor ...)".
--   Questo permette a 'applyCliffordToRotation' di vedere la Pauli String completa.
normalizeRotations :: QOp -> QOp
normalizeRotations op = case op of
    -- Caso: Unisco due rotazioni con stesso angolo (R P1 t ⊗ R P2 t) -> R (P1⊗P2) t
    Tensor (R p1 t1) (R p2 t2) 
        | t1 == t2 -> R (Tensor p1 p2) t1
        
    -- Caso: Rotazione a sinistra, Identità a destra
    Tensor (R p1 t1) I -> R (Tensor p1 I) t1
    Tensor (R p1 t1) One -> R (Tensor p1 One) t1 -- Gestione One come I
    
    -- Caso: Identità a sinistra, Rotazione a destra
    Tensor I (R p2 t2) -> R (Tensor I p2) t2
    Tensor One (R p2 t2) -> R (Tensor One p2) t2
    
    -- Ricorsione standard
    Tensor a b -> 
        let a' = normalizeRotations a
            b' = normalizeRotations b
        in case (a', b') of
             -- Riprova il matching dopo aver normalizzato i figli
             (R p1 t1, R p2 t2) | t1 == t2 -> R (Tensor p1 p2) t1
             (R p1 t1, I) -> R (Tensor p1 I) t1
             (I, R p2 t2) -> R (Tensor I p2) t2
             _ -> Tensor a' b'

    Compose a b -> Compose (normalizeRotations a) (normalizeRotations b)
    C a -> C (normalizeRotations a)
    -- Aggiungi altri costruttori se necessario...
    _ -> op

-- | Aggiorna splitCircuit per includere la normalizzazione
splitCircuit :: QOp -> QOp
splitCircuit op = 
    let normalizedOp = normalizeRotations op -- <--- PASSO AGGIUNTO
        (rots, cliffs) = pushCliffordsToEnd normalizedOp
    in Compose cliffs rots

-- Helper per stampare in modo leggibile
runVisualTest :: String -> QOp -> IO ()
runVisualTest name circuit = do
    putStrLn $ "\n========================================"
    putStrLn $ "TEST: " ++ name
    putStrLn "----------------------------------------"
    putStrLn "1. CIRCUITO ORIGINALE (Misto):"
    pp circuit
    putStrLn "\n2. CIRCUITO SEPARATO (Clifford . Rotazioni):"
    let separated = splitCircuit circuit
    pp separated
    putStrLn "========================================\n"

-- ==========================================
-- SCENARI DI TEST
-- ==========================================

-- CASO 1: Inversione di Segno (Phase Flip)
-- Fisica: Rz(theta) * X = X * Rz(-theta)
-- Se applichi X e poi ruoti Z, è come ruotare Z al contrario e poi applicare X.
testSignFlip :: IO ()
testSignFlip = runVisualTest "Segno Meno (X vs Rz)" $
    Compose (R Z 1.0) X 
    -- Nota: Compose A B applica B poi A.
    -- Qui: Applica X, POI Rz.
    -- Risultato atteso: Compose X (R Z -1.0) -> Applica Rz(-1) POI X.

-- CASO 2: Cambio di Base (Hadamard)
-- Fisica: Rz(theta) * H = H * Rx(theta)
-- La rotazione Z, passando attraverso H, deve diventare rotazione X.
testBasisChange :: IO ()
testBasisChange = runVisualTest "Cambio Base (H vs Rz)" $
    Compose (R Z 0.5) H
    -- Qui: Applica H, POI Rz.
    -- Risultato atteso: Compose H (R X 0.5) -> Applica Rx POI H.

-- CASO 3: Propagazione CNOT (Controllo)
-- Fisica: Rx(theta) sul Controllo * CNOT = CNOT * Rxx(theta)
-- Una rotazione X sul controllo "contagia" il target diventando XX.
testCNOTControl :: IO ()
testCNOTControl = runVisualTest "CNOT Propagazione (X su Controllo)" $
    let 
        cnot = C X             -- CNOT standard (Controllo 0, Target 1)
        rotX = Tensor (R X 0.7) I -- Rotazione X su Qubit 0
    in Compose rotX cnot
    -- Qui: Applica CNOT, POI Rx su Q0.
    -- Attenzione all'ordine di push: vogliamo Cliffords (CNOT) alla fine.
    -- CNOT deve "attraversare" Rx andando verso sinistra? 
    -- No, splitCircuit spinge i Clifford verso la FINE temporale (sinistra nella composizione).
    -- Se input è (Rx . CNOT), il CNOT è già a destra (inizio temporale).
    -- Proviamo il contrario: Rx deve "saltare" oltre il CNOT per andare a destra.
    -- Input: Compose CNOT (Rx su Controllo) -> Applica Rx, poi CNOT.
    -- Vogliamo: CNOT . Rx'

-- Riprova CASO 3 corretto per l'algoritmo:
-- Input: Compose (C X) (Tensor (R X 0.7) I)
-- Significato: Applica Rx(0.7) su Q0, POI applica CNOT.
-- Poiché CNOT è Clifford, splitCircuit lo lascerà lì e cercherà di portare altri Clifford a sinistra.
-- Questo è già separato! 
-- Dobbiamo testare il caso "Sbagliato" che deve essere corretto:
-- Vogliamo applicare un Clifford DOPO, e vederlo spostato PRIMA?
-- No, vogliamo separare Cliff (futuro) da Rot (passato).

testCNOTPropagation :: IO ()
testCNOTPropagation = runVisualTest "CNOT Propagation (X through CNOT)" $
    -- Input: Applica CNOT, POI applica Rz sul Target.
    -- Op: Rz_target . CNOT
    -- Vogliamo spostare CNOT "fuori" verso sinistra (dopo).
    -- Ma CNOT è già a sinistra!
    
    -- L'algoritmo splitCircuit fa: (Cliffords . Rotazioni).
    -- Cioè: Esegui Rotazioni, POI Cliffords.
    
    -- Scenario interessante:
    -- Ho una rotazione che avviene DOPO un Clifford. Voglio spostare il Clifford DOPO la rotazione.
    -- Op: CNOT . Rz_target
    -- (Esegui Rz, poi CNOT). Questo è già il formato target.
    
    -- Scenario "Da Aggiustare":
    -- Esegui CNOT, POI Rz_target.
    -- Op: Rz_target . CNOT
    -- Dobbiamo portare CNOT a sinistra (dopo Rz).
    -- Rz deve passare "sotto" CNOT verso destra (prima).
    -- Z sul target commuta con CNOT (ZZ -> IZ ? No).
    -- Z sul target con CNOT: CNOT (I x Z) = (Z x Z) CNOT ?
    -- Controlliamo: CNOT (I x Z) |00> -> CNOT |00> -> |00> -> Z target -> |00>
    -- (Z x Z) CNOT |00> -> ZZ |00> -> |00>
    -- CNOT (I x Z) |10> -> CNOT -|10> -> -|11> -> Z target -> -(-|11>) = |11>
    -- (Z x Z) CNOT |10> -> ZZ |11> -> (-1)(-1)|11> = |11>.
    -- Sì, Z su target diventa ZZ.
    
    Compose (C X) (Tensor I (R Z 0.5))
    -- Input: Applica Rz(target), POI CNOT. 
    -- Aspetta... Compose A B = Applica B poi A.
    -- Qui B = Rz(target), A = CNOT.
    -- Questo è GIA' nel formato (Cliff . Rot).
    -- Dobbiamo testare il caso inverso:
    -- Compose (R... ) (C X) -> Applica CNOT, poi R.
    
    --Compose (Tensor I (R Z 0.5)) (C X)
    -- Input: Applica CNOT, POI Rz su target.
    -- Output Atteso: Applica Rzz(0.5) POI CNOT.
    -- Visivamente: Compose (C X) (R (Tensor Z Z) 0.5)

-- CASO 4: Catena Lunga
testChain :: IO ()
testChain = runVisualTest "Catena Lunga (H . X . Rz)" $
    -- Ordine esecuzione: Rz, poi X, poi H
    -- Formato input: Compose H (Compose X (R Z 0.3))
    -- Risultato atteso: I clifford H e X si fondono a sinistra.
    -- La rotazione Z passa attraverso X (diventa -Z) e H (diventa -X).
    -- Finale: (H.X) . R(-X)
    Compose H (Compose X (R Z 0.3))

-- CASO 3 AGGRESSIVO: CNOT deve saltare "sopra" Rz
-- Input: Esegui CNOT, poi Rz. 
-- L'algoritmo deve trasformarlo in: Esegui R(ZZ) poi CNOT.
testCNOTJump :: IO ()
testCNOTJump = runVisualTest "CNOT Salta Rz (Propagazione)" $
    -- Compose (R ...) (C X) -> Esegui CNOT, poi R
    Compose (Tensor I (R Z 0.5)) (C X)

-- CASO 4 AGGRESSIVO: Catena Inversa
-- Input: Esegui X, poi H, POI Rz.
-- L'algoritmo deve portare X e H alla fine (dopo Rz).
-- Rz dovrà subire due trasformazioni:
-- 1. Attraverso H -> Diventa Rx
-- 2. Attraverso X -> Rx commuta con X (nessun cambio base, ma controlliamo il segno)
testChainJump :: IO ()
testChainJump = runVisualTest "Cliffords Saltano Rz" $
    -- Compose (Rotazione) (Cliffords)
    Compose (R Z 0.3) (Compose H X)


-- ==========================================
-- HELPER PER COSTRUIRE CIRCUITI GRANDI
-- ==========================================
-- Questi helper ci evitano di scrivere "Tensor I (Tensor I ...)" a mano.

-- | Crea un gate H sull'i-esimo qubit in un sistema di n qubit
h :: Int -> Int -> QOp
h i n 
    | i == 0 = Tensor H (nId (n-1))
    | otherwise = Tensor I (h (i-1) (n-1))

-- | Crea un gate X sull'i-esimo qubit
x :: Int -> Int -> QOp
x i n 
    | i == 0 = Tensor X (nId (n-1))
    | otherwise = Tensor I (x (i-1) (n-1))

-- | Crea un CNOT tra i e i+1
cx :: Int -> Int -> QOp
cx i n = adjacentCNOT i n

-- | Crea una rotazione RZ sull'i-esimo qubit
rz :: Int -> Int -> RealT -> QOp
rz i n theta 
    | i == 0 = Tensor (R Z theta) (nId (n-1))
    | otherwise = Tensor I (rz (i-1) (n-1) theta)

-- ==========================================
-- STRESS TESTS
-- ==========================================

-- 1. PING PONG TEST (Identità)
-- Fisica: H * H = I. 
-- Input: Esegui H, poi H, poi Rz.
-- Struttura: Rz . H . H
-- L'algoritmo sposterà i due H "a sinistra" (dopo Rz).
-- Rz -> (attraverso H) -> Rx -> (attraverso H) -> Rz.
-- Risultato atteso: Rz (invariato o con segno), e i Clifford spostati.
testPingPong :: IO ()
testPingPong = runVisualTest "PING PONG (H . H . Rz)" $
    let n = 1
        cliffords = Compose (h 0 n) (h 0 n) -- H poi H
        rotation  = rz 0 n 0.5              -- Rz
    in Compose rotation cliffords

-- 2. INFEZIONE CNOT (Spreading)
-- Fisica: Una rotazione X sul controllo di una catena di CNOT si propaga.
-- Input: CNOT(0,1) -> CNOT(1,2) -> R_x(0)
-- Struttura: R_x(0) . CNOT(1,2) . CNOT(0,1)
-- Attenzione all'ordine Compose: (Compose A B) esegue B poi A.
-- Vogliamo eseguire i CNOT *prima* della rotazione, e farli saltare *dopo*.
-- Input Haskell: Compose (Rotation) (Cliffords)
testSpreading :: IO ()
testSpreading = runVisualTest "INFEZIONE (X attraversa CNOTs)" $
    let n = 3
        -- Esegui CNOT 0->1, poi CNOT 1->2
        cliffs = Compose (cx 1 n) (cx 0 n) 
        -- Poi esegui Rx su 0
        rot    = Tensor (R X 0.9) (nId 2)
    in Compose rot cliffs

-- 3. CANCELLAZIONE PARZIALE (Commutazione)
-- Fisica: Z sul target di un CNOT (Control=X) commuta?
-- CNOT = |0><0|I + |1><1|X. 
-- Z sul target: CNOT (I x Z).
-- Se controllo è 0: |0> Z|t>. Se controllo 1: X Z |t>.
-- (I x Z) CNOT: Se 0: |0> Z|t>. Se 1: Z X |t> = - X Z |t>.
-- Quindi Z sul target ANTICOMMUTA con CNOT se visto come operatore globale?
-- No, Z target diventa Z target * Z controllo (ZZ).
-- Verifichiamo cosa dice il tuo codice.
testTargetZ :: IO ()
testTargetZ = runVisualTest "TARGET Z (Z attraversa CNOT)" $
    let n = 2
        cliffs = cx 0 n -- CNOT 0->1
        rot    = rz 1 n 0.5 -- Rz su 1
    in Compose rot cliffs

-- 4. IL "PANINO" SWAP (Mixed Tensor)
-- Uno SWAP è composto da 3 CNOT: CX(0,1)-CX(1,0)-CX(0,1).
-- Se faccio passare un Rz(0) attraverso uno SWAP, deve diventare Rz(1).
-- Nota: Il tuo 'adjacentCNOT' fa solo CX(i, i+1). Per fare CX(1,0) servirebbe H su entrambi i lati o logica extra.
-- Simuliamo invece un "Semi-Swap": CX(0,1) - CX(1,0 con H).
-- Facciamo una cosa più semplice supportata dal tuo codice attuale:
-- H(0) . H(1) . Rz(0) . H(1) . H(0)
-- Questo dovrebbe trasformare Rz -> Rx (primo H) -> Rx (passa H su 1) -> Rz (secondo H).
testSandwich :: IO ()
testSandwich = runVisualTest "PANINO H (H0 H1 Rz0 H1 H0)" $
    let n = 2
        -- Cliffords: H0, H1
        cliffs = Compose (h 1 n) (h 0 n)
        -- Rotazione Rz su 0
        rot = rz 0 n 0.3
        -- Altri Cliffords dopo? No, testiamo solo il salto "in avanti".
        -- Costruiamo: Esegui cliffs, poi rot. Sposta cliffs DOPO rot.
    in Compose rot cliffs
-- ==========================================
-- HELPER PER COSTRUIRE CIRCUITI GRANDI
-- ==========================================

-- | Crea un gate H sull'i-esimo qubit in un sistema di n qubit
h :: Int -> Int -> QOp
h i n 
    | i == 0 = Tensor H (nId (n-1))
    | otherwise = Tensor I (h (i-1) (n-1))

-- | Crea un gate X sull'i-esimo qubit
x :: Int -> Int -> QOp
x i n 
    | i == 0 = Tensor X (nId (n-1))
    | otherwise = Tensor I (x (i-1) (n-1))

-- | Crea un CNOT tra i e i+1
cx :: Int -> Int -> QOp
cx i n = adjacentCNOT i n

-- | Crea una rotazione RZ sull'i-esimo qubit
--   RINOMINATA in 'rzOp' per evitare conflitti con Programs.QFT.rz
rzOp :: Int -> Int -> RealT -> QOp
rzOp i n theta 
    | i == 0 = Tensor (R Z theta) (nId (n-1))
    | otherwise = Tensor I (rzOp (i-1) (n-1) theta)

-- ==========================================
-- STRESS TESTS
-- ==========================================

-- 1. PING PONG TEST (Identità)
testPingPong :: IO ()
testPingPong = runVisualTest "PING PONG (H . H . Rz)" $
    let n = 1
        cliffords = Compose (h 0 n) (h 0 n) 
        rotation  = rzOp 0 n 0.5            -- Usa rzOp qui
    in Compose rotation cliffords

-- 2. INFEZIONE CNOT (Spreading)
testSpreading :: IO ()
testSpreading = runVisualTest "INFEZIONE (X attraversa CNOTs)" $
    let n = 3
        cliffs = Compose (cx 1 n) (cx 0 n) 
        rot    = Tensor (R X 0.9) (nId 2) -- Rx qui è definito manualmente, ok
    in Compose rot cliffs

-- 3. CANCELLAZIONE PARZIALE (Commutazione)
testTargetZ :: IO ()
testTargetZ = runVisualTest "TARGET Z (Z attraversa CNOT)" $
    let n = 2
        cliffs = cx 0 n 
        rot    = rzOp 1 n 0.5               -- Usa rzOp qui
    in Compose rot cliffs

-- 4. IL "PANINO" SWAP (Mixed Tensor)
testSandwich :: IO ()
testSandwich = runVisualTest "PANINO H (H0 H1 Rz0 H1 H0)" $
    let n = 2
        cliffs = Compose (h 1 n) (h 0 n)
        rot = rzOp 0 n 0.3                  -- Usa rzOp qui
    in Compose rot cliffs
