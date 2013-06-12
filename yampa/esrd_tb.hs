-- esrd_tb.hs
--
-- Oliver Schneider
-- University of Saskatchewan
--
-- An implementation of a multi-state agent-based Tuberculosis and CKD simulation

{-# LANGUAGE Arrows #-}

module ESRD_TB where

import WXFruit
import FRP.Yampa hiding (left, right, next)
import Control.Arrow hiding (left, right)
import Maybe
import List (find)
import Random (mkStdGen, random, randomR, RandomGen)

--------------------------------------
--			 GENERAL MODEL			--
--------------------------------------

-- Data definitions
data TBState = TBSus | TBActiveUD | TBActiveD | LITBI
	deriving(Eq)
type CKDState = Int -- This is a temporary setup - seems a waste to declare a new data type
data Message = ForceTB | ContactTB --temporary
	deriving(Eq)

data State = Dead | Alive TBState CKDState [Key]
type Key = (Int, Int)
type Seed = Int

type TBSF = SF (CKDState, [Message]) TBState
type CKDSF = SF (Event CKDState) CKDState
type Cell = SF (Event [(Key, Message)]) ((Key, State), Event (Key, Message))
type Cells =  [Cell]
type States = [(Key, State)]

--constants
boardSize = 800
nCells = 200
totalCells = fromIntegral nCells
neighbourSize = (2,5)
--params
infectionProbability, primaryProgression, normReactivationRate, diagnosisTilRecovery :: Double
timeUntilDiagnosis, meanTimeClearInfection :: Time
infectionProbability = 1--0.5 -- per contact
primaryProgression = 0.1 -- when infected, immediately have TB
timeUntilDiagnosis = 60
meanTimeClearInfection = 180
normReactivationRate = 0.1 --0.01
diagnosisTilRecovery = 30
connectivity = 0.03

--init
board2 :: Seed -> Cells
board2 s = [cell (x, y) (filter (/=(x,y)) neighbs) (tb_sf seed (x,y)) (ckd_sf seed) | (((x,y), seed), neighbs) <- list]
	where
		list = (coords `zip` seeds) `zip` nList
		coords = take nCells (xs `zip` ys)
		xs = randomRs (0, boardSize-1) (mkStdGen (s+1))
		ys = randomRs (0, boardSize-1) (mkStdGen (s+2))
		seeds = randoms (mkStdGen s)
		connectProbs = randomRs (0,1) (mkStdGen (s+1))
		crossProd = [(a, b) | a <- coords, b <- coords, b > a]
		connections = map fst $ filter ( (<(connectivity::Double)) . snd) $ crossProd `zip` connectProbs --truly random connections
		nList = map getN coords
		getN k = [ f | (f,s) <- connections, s == k] ++ [ s | (f,s) <- connections, f == k ]		
		tb_sf s k = if k == coords!!0 || k == coords!!1 then tb_ActiveUD s else tb_Susceptible s


board :: Seed -> Cells
board s =  [cell (x, y) (get_neighbours x y) (startSF seed x y) (ckd_sf seed) | ((x,y), seed) <- list] --values
	where
		list = coords `zip` rList
		coords = [(x,y) | x <- [0..boardSize-1], y <- [0..boardSize-1]]
		rList = randoms (mkStdGen s)
		--starting board for testing
		startSF seed 0 0 = tb_ActiveUD seed
		startSF seed 4 6 = tb_ActiveUD seed
		startSF seed 9 10 = tb_ActiveUD seed
		startSF seed 15 12 = tb_ActiveUD seed
		startSF seed 4 19 = tb_ActiveUD seed
		startSF seed  _ _	= tb_Susceptible seed
		--no wraparound
		get_neighbours x y = [(x', y') | x' <- [x-1,x,x+1], y' <- [y-1,y,y+1], (x',y') /= (x,y), inBounds x' y']
		inBounds a b = a >= 0 && a < boardSize && b >= 0 && b < boardSize --change the comparators for 0 to be non-strict (when implementing random neighbours)


-- main logic
cell :: Key -> [Key] -> TBSF -> CKDSF -> Cell
cell (x,y) neighbours tb_sf ckd_sf = proc input -> do
		rec
			let id = (x,y)
			let extract_msgs = map snd . filter ((==id) . fst)
			let inp = event [] extract_msgs input
			let n = length neighbours
			ckd_s <- ckd_sf <<< edgeTag 0 -< dead
			tb_s <- tb_sf -< (ckd_s, inp)
			dead <- dHold False <<< rate (mkStdGen (boardSize * x + y)) (ckdDeathChance 1) True -< ckdDeathChance ckd_s
			index <- noiseR (0, (length neighbours)-1) (mkStdGen (boardSize * x + y)) -< () --need to find better way to select neighbours
			let target = if (tb_s == TBActiveUD || tb_s == TBActiveUD) && (n > 0 ) 
							then Event (neighbours!!index, ContactTB) 
							else noEvent --TODO: other types of messages
		returnA -< if dead then ((id, Dead), noEvent) else ((id, (Alive tb_s ckd_s neighbours)), target)




rate :: RandomGen g => g -> SF (Double, a) (Event a) --Need to change this to SF (Double, a) (Event a)
rate g = dSwitch (timeout t &&& sf) next
	where
		sf = arr dup >>> first (arr (/= l) >>> edge) >>> arr (uncurry tag)
		next l' = rate g' l' a
		(r, g') = randomR (0,1) g
		t = - (log (1 - r)) / l
		timeout t = now () >>> delayEvent t

{-
rate :: RandomGen g => g -> Double -> a -> SF Double (Event a) --Need to change this to SF (Double, a) (Event a)
rate g l a = dSwitch (timeout t a &&& sf) next
	where
		sf = arr dup >>> first (arr (/= l) >>> edge) >>> arr (uncurry tag)
		next l' = rate g' l' a
		(r, g') = randomR (0,1) g
		t = - (log (1 - r)) / l
		timeout t a = now a >>> delayEvent t
-}		
{-
rate :: RandomGen g => g -> SF (Double, a) (Event a)
rate g = proc (n, a) -> do
		let (r, _) = randomR (0,1) g
		let t = - (log (1 - r)) / n
		e <- delayEvent t -< ()
		returnA -< e `tag` a
-}

--------------------------------------
--		  TB states of agents		--
--------------------------------------

tb_Susceptible :: Seed -> TBSF
tb_Susceptible seed = dSwitch (constant TBSus &&& sf ) find_next
			where
				sf = proc (_, list) -> do
					let fb = isJust (find (==ForceTB) list)
					let cb = isJust (find (==ContactTB) list)
					e <- edge -< fb || (cb && (r1 < infectionProbability))
					returnA -< if fb then e `tag` ForceTB else e `tag` ContactTB
				find_next ForceTB	= tb_LTBI r3
				find_next ContactTB = if (r2 < primaryProgression) then (tb_ActiveUD r3) else (tb_LTBI r3)
				(r1, g1) = randomR (0, 1) (mkStdGen seed) --switch to noiseR
				(r2, g2) = randomR (0, 1) g1
				(r3, _) = random g2
				

--TODO: Send messages to neighbours
tb_ActiveUD :: Seed -> TBSF
tb_ActiveUD seed = dSwitch (constant TBActiveUD &&& sf ) id
			where
				sf = proc list -> do
					diag <- delayEvent timeUntilDiagnosis <<< now (tb_ActiveD r) -< ()
					--r <- noiseR (0,1) (mkStdGen seed) -< ()
					symp_clear <- rate (mkStdGen r) (1/meanTimeClearInfection) (tb_LTBI r)-< 1/meanTimeClearInfection
					returnA -< diag `lMerge` symp_clear
				(r, _) = random (mkStdGen seed)


--TODO: Send messages to neighbours
tb_LTBI :: Seed -> TBSF
tb_LTBI seed = dSwitch sf find_next
	where
		sf = arr fst >>> (constant LITBI &&& e)
		e = (arr activeRate) >>> rate (mkStdGen seed) (activeRate 1)  ()
		find_next _ = tb_ActiveUD r
		(r, _) = random (mkStdGen seed)


--TODO: Send messages to neighbours
tb_ActiveD :: Seed -> TBSF
tb_ActiveD seed = dSwitch (constant TBActiveD &&& sf) find_next
	where
		sf = now () >>> delayEvent diagnosisTilRecovery
		find_next _ = tb_LTBI r
		(r, _)	= random (mkStdGen seed)


--------------------------------------
--		  CKD states of agents		--
--------------------------------------

ckd_sf :: Seed -> CKDSF 
ckd_sf seed = proc input -> do
	rec
		n <- dHold 1 -< input `lMerge` e
		let trans = ckdTrans n
		e <- rate (mkStdGen seed) (fst $ ckdTrans 1) 2 -< fst trans
	returnA -< n
		


ckdTrans :: CKDState -> (Double, CKDState)
ckdTrans st 
	| st == 1	= (0.0074, 2)
	| st == 2	= (0.2023, 3)
	| st == 3	= (0.1245, 4)
	| st == 4	= (0.0629, 5)
	| st == 5	= (0.1015, 6)
	| st == 6	= (0.1648, 7)
	| st == 7	= (0.05, 8)
	| st == 8	= (0.097, 7)
	| otherwise	= (0,0)
	
	
	
ckdDeathChance :: CKDState -> Double
ckdDeathChance st 
	| st == 1	= 0.0133
	| st == 2	= 0.0712
	| st == 3	= 0.1232
	| st == 4	= 0.1842
	| st == 5	= 0.2174
	| st == 6	= 0.1923
	| st == 7	= 0.2222
	| st == 8	= 1/20
	| otherwise	= 0


--------------------------------------
--			  INTERACTION			--
--------------------------------------

activeRate :: CKDState -> Double
activeRate st = normReactivationRate * coeff
	where
		coeff
			| st > 0 && st <= 3	= 1
			| st <= 7			= 7
			| st == 8			= 50
			| otherwise			= 0



--------------------------------------
--				MAIN				--
--------------------------------------

--runs the game with a step time of 'step'
runGame :: Seed -> Time -> SF () States
runGame seed step = proc _ -> do
		rec
			e <- repeatedly step () -< ()
			let ev = catEvents events
			results <- parB (board2 seed) -< e `tag` (event [] id ev)
			let (kstates, events) = unzip results
		returnA -< kstates


