-- SIR.hs
--
-- Oliver Schneider
-- University of Saskatchewan
--
-- An implementation of a simple agent-based susceptible/infected/recovered epidemic model

{-# LANGUAGE Arrows #-}

module Main where

import WXFruit
import FRP.Yampa hiding (left, right, next)
import Control.Arrow hiding (left, right)
import Graphics.UI.WX as WX hiding (Key, Event, drawRect)
import Maybe
import Random (mkStdGen)

--------------------------------------
--				MODEL				--
--------------------------------------

--Data definitions
data State = Sus | Inf | Rec
	deriving(Eq)
type StateSF = SF [Key] State
type Key = (Int, Int)
type Cell = SF (Event [Key]) ((Key, State), Event Key)
type CellGrid =  [Cell]
type StateGrid = [(Key, State)]

--constants
boardSize = 50
defRecTime :: Time
defRecTime = 3
defSusTime :: Time
defSusTime = 2

--init
board :: CellGrid
board =  [cell (x, y) defRecTime (get_neighbours x y) (startSF x y) | x <- [0..boardSize-1], y <- [0..boardSize-1]] --values
	where
		--starting board for testing
		startSF 0 0 = infected
		startSF _ _ 	= susceptible
		--no wraparound
		get_neighbours x y = [(x', y') | x' <- [x-1,x,x+1], y' <- [y-1,y,y+1], (x',y') /= (x,y), inBounds x' y']
		inBounds a b = a >= 0 && a < boardSize && b >= 0 && b < boardSize


-- main logic
cell :: Key -> Time -> [Key] -> StateSF -> Cell
cell (x,y) recTime neighbours sf = proc input -> do
		let id = (x,y)
		let inp = event [] (filter (==id)) input
		state <- sf -< inp
		index <- noiseR (0, (length neighbours)-1) (mkStdGen (boardSize * x + y)) -< ()
		let target = if state == Inf then Event (neighbours!!index) else noEvent
		returnA -< ( (id, state), target) --placeholder


--states of agents
susceptible :: StateSF
susceptible = dSwitch (constant Sus &&& (arr (/=[]) >>> edge)) (\_ -> infected)

infected :: StateSF
infected = dSwitch (constant Inf &&& (now () >>> delayEvent defRecTime)) (\_ -> recovered)

recovered :: StateSF
recovered = dSwitch (constant Rec &&& (never >>> delayEvent defRecTime)) (\_ -> susceptible)


--runs the game with a step time of 'step'
runGame :: Time -> SF () StateGrid
runGame step = proc _ -> do
		rec
			e <- repeatedly step () -< ()
			let ev = catEvents events
			results <- parB board -< e `tag` (event [] id ev)
			let (kstates, events) = unzip results
		returnA -< kstates

--------------------------------------
--				VIEW				--
--------------------------------------

--constants/dimensions
cellWidth = 12
screenDim = cellWidth * boardSize
screenSize = WX.sz screenDim screenDim

--main background
bg :: WXPicture
bg = wxWithColor WX.white wxfill

--draws the board
drawBoard :: StateGrid -> WXPicture
drawBoard = foldl drawCell wxblank
	where
		drawCell pic ((x,y), state) = pic `wxPicOver` drawRect (colourMap state) (x * cellWidth) (y*cellWidth) (cellWidth-1) (cellWidth-1)
		
colourMap :: State -> WX.Color
colourMap Sus = WX.yellow
colourMap Inf = WX.red
colourMap Rec = WX.blue

--the main window
mainGUI :: WXGUI () ()
mainGUI = wxVBox ( proc _ -> do
		state <- wxBoxSF (runGame 0.1) -< ()
		_ <- wxpicture (psize screenSize) -< ppic (drawBoard state `wxPicOver` bg)
		returnA -< () )

--main entry point to program
main = startGUI "Susceptible - Infected - Recovered" mainGUI


--------------------------------------
--				UTIL				--
--------------------------------------

drawRect :: WX.Color -> Int -> Int -> Int -> Int -> WXPicture
drawRect col x y w h = wxWithColor col $ wxPicFill $ wxrect box
	where box = WX.rect (WX.Point x y) (WX.sz w h)

