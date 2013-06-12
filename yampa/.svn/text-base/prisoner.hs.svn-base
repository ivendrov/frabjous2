-- prisoner.hs
--
-- Oliver Schneider
-- University of Saskatchewan
--
-- An implementation of the spatial prisoner's dilemma in Yampa FRP

{-# LANGUAGE Arrows #-}

module Prisoner where


import FRP.Yampa hiding (left, right, next)
import FRP.Yampa.Utilities (parZ)

import Control.Arrow hiding (left, right)
import Data.Array
import Random

----------------------------------
--		Data definitions		--
----------------------------------

data Action = Coop | Defect | None
	deriving(Eq)
data Strat = AlwaysCoop | AlwaysDefect | TitForTat | Dead
	deriving(Eq, Enum)
type Energy = Int
type State = (Strat, [Action], Energy)
type Strategy = SF (Event Action) Action
type Cell = SF (Event StateGrid) State
type CellBoard = Array (Int, Int) Cell
type StateGrid = Array (Int, Int) State


----------------------------------
--			Constants			--
----------------------------------
boardSize = 20
nSize = 8

board :: StdGen -> CellBoard
board g =  array	( (0,0),	(boardSize-1, boardSize-1) ) --dimensions
					[((x,y), defaultCell x y (getStrat x y)) | x <- [0..boardSize-1], y <- [0..boardSize-1]] --values
	where
		getStrat x y =  toEnum ( rList!!(x * boardSize +y) `mod` 3) :: Strat	
		rList = randoms g


--parameters

--change in energy based on one's action and one's partner's action
--maybe switch this to a parameterized function when modularizing?
reward :: Action -> Action -> Energy
reward _ None		= 0
reward None _ 		= 0
reward Coop Coop	= 1
reward Coop _ 		= -2
reward _ Coop		= 2
reward _ _ 			= -1


----------------------------------
--			Main Logic			--
----------------------------------

defaultCell = cell 100 1 --starts off with 100 energy, loses 1 energy per step just for living

cell :: Energy -> Energy -> Int -> Int -> Strat -> Cell
cell iEnergy eDecay x y iStrat = proc input -> do
		rec 
			let neighbours = get_neighbours x y input
			strats <- parZ (replicate nSize (strategyFromStrat iStrat)) -< map (input `tag`) neighbours
			let dEnergy = sum $ map (uncurry reward) $ zip strats neighbours
			energy <- switch (dHold iEnergy >>> (identity &&& (arr (<=0) >>> edge))) (\_ -> constant 0) -< input `tag` (energy + dEnergy - eDecay)
		returnA -< if (energy <= 0) then (Dead, (replicate nSize None), 0) else (iStrat, strats, energy)


strategyFromStrat :: Strat -> Strategy
strategyFromStrat AlwaysCoop = alwaysCoop
strategyFromStrat AlwaysDefect = alwaysDefect
strategyFromStrat TitForTat = titForTat
strategyFromStrat Dead = dead

--strategies
alwaysCoop :: Strategy
alwaysCoop = constant Coop

alwaysDefect :: Strategy
alwaysDefect = constant Defect

titForTat :: Strategy
titForTat = dHold Coop

dead :: Strategy
dead = constant None


--returns a list of all strats in a Moore neighbourhood
get_neighbours :: Int -> Int -> Event StateGrid -> [Action]
get_neighbours _ _ NoEvent = replicate nSize None
get_neighbours x y (Event world) = [ actions!!(flipIndex n) | ((_, actions, _), n) <- (zip stateList [1..]) ]
			where
				stateList = [world!(x', y')	| x' <- wrapList x, y' <- wrapList y, (x',y') /= (x,y)]
				--wraparound
				wrapList n
					| n == 0			= [boardSize-1, n, n+1]
					| n == boardSize-1	= [n-1, n, 0]
					| otherwise			= [n-1, n, n+1]
				flipIndex = (nSize -) --needs wraparound
				


--runs the game with a step time of 'step'
runGame :: StdGen -> Time -> SF () StateGrid
runGame g step = proc _ -> do
		rec
			e <- repeatedly step () -< ()
			world <- parB (board g) -< e `tag` world
		returnA -< world
		

--------------------------------------
--				UTIL				--
--------------------------------------

middle3 :: (a,b,c) -> b
middle3 (a,b,c) = b

splitE3 :: Event(a,b,c) -> (Event a, Event b, Event c)
splitE3 NoEvent			= (NoEvent, NoEvent, NoEvent)
splitE3 (Event (a,b,c)) = (Event a, Event b, Event c)

