-- GameOfLife.hs
--
-- Oliver Schneider
-- University of Saskatchewan
--
-- An implementation of the game of life in Yampa FRP

{-# LANGUAGE Arrows #-}

module Main where

import WXFruit
import FRP.Yampa hiding (left, right, next)
import Control.Arrow hiding (left, right)
import qualified Graphics.UI.WX as WX
import Data.Array
import Data.Maybe

--------------------------------------
--				MODEL				--
--------------------------------------

--Data definitions
type Cell = SF (Event LifeState) Bool
type LifeBoard = Array (Int, Int) Cell
type LifeState = Array (Int, Int) Bool

--constants
boardSize :: Int
boardSize = 20

board :: LifeBoard
board =  array	( (0,0),	(boardSize-1, boardSize-1) ) --dimensions
				[((x,y), cell x y (startCond x y)) | x <- [0..boardSize-1], y <- [0..boardSize-1]] --values
	where
--starting board for testing
		startCond 2 2 = True
		startCond 3 2 = True
		startCond 4 2 = True
		startCond 5 5 = True
		startCond 6 6 = True
		startCond 6 7 = True
		startCond 5 7 = True
		startCond 4 7 = True
		startCond _ _ 	= False
		

--parameters
--currently standard Conway's Life
birthCond :: Int -> Bool
birthCond = (==3)

liveCond :: Int -> Bool
liveCond n 
	| n == 2	= True
	| n == 3	= True
	| otherwise	= False


-- main logic
cell :: Int -> Int -> Bool -> Cell
cell x y init = proc input -> do
		rec 
			let neighbours = get_neighbours x y input
			let nCount = length $ filter (==True) neighbours
			alive <- dHold init -< input `tag` (birthCond nCount || (alive && liveCond nCount))
		returnA -< alive
		
--returns a list of all neighbours in the world		
get_neighbours :: Int -> Int -> Event LifeState -> [Bool]
get_neighbours _ _ NoEvent = []
get_neighbours x y (Event world) = [world!(x', y')	| x' <- [x-1,x,x+1], y' <- [y-1,y,y+1], (x',y') /= (x,y), inBounds x' y']
			where
				inBounds a b = a > 0 && a < boardSize && b > 0 && b < boardSize

--runs the game with a step time of 'step'
runGame :: Time -> SF () LifeState
runGame step = proc _ -> do
		rec
			e <- repeatedly step () -< ()
			world <- parB board -< e `tag` world
		returnA -< world

--------------------------------------
--				VIEW				--
--------------------------------------

--constants/dimensions
cellWidth = 15
screenDim = cellWidth * boardSize
screenSize = WX.sz screenDim screenDim

--main background
bg :: WXPicture
bg = wxWithColor WX.white wxfill

--draws the board
drawBoard :: LifeState -> WXPicture
drawBoard state = foldl drawCell wxblank $ indices state
	where
		drawCell pic (x,y) = pic `wxPicOver` drawRect (colour  (x,y)) (x * cellWidth) (y*cellWidth) (cellWidth-1) (cellWidth-1)
		colour k = if state!k then WX.red else WX.blue

--the main window
mainGUI :: WXGUI () ()
mainGUI = wxVBox ( proc _ -> do
		state <- wxBoxSF (runGame 0.1) -< ()
		_ <- wxpicture (psize screenSize) -< ppic (drawBoard state `wxPicOver` bg)
		returnA -< () )

--main entry point to program
main = startGUI "The Game of Life" mainGUI


--------------------------------------
--				UTIL				--
--------------------------------------

drawRect :: WX.Color -> Int -> Int -> Int -> Int -> WXPicture
drawRect col x y w h = wxWithColor col $ wxPicFill $ wxrect box
	where box = WX.rect (WX.Point x y) (WX.sz w h)

