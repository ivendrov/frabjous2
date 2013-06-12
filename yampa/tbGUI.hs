-- tbGUI.hs
--
-- Oliver Schneider
-- University of Saskatchewan
--
-- An implementation of a multi-state agent-based Tuberculosis and CKD simulation

{-# LANGUAGE Arrows #-}

module Main where

import WXFruit
import FRP.Yampa hiding (left, right, next)
import Control.Arrow hiding (left, right)
import Graphics.UI.WX as WX hiding (Key, Event, drawRect)
import Maybe
import ESRD_TB

--------------------------------------
--				VIEW				--
--------------------------------------

--constants/dimensions
cellWidth = 20 --OLD/GRID: 40
screenDim = boardSize + cellWidth --OLD/GRID: boardSize * cellWidth
screenSize = WX.sz screenDim screenDim
seed = 12

--main background
bg :: WXPicture
bg = wxWithColor WX.white wxfill

--draws the board
drawBoard :: States -> WXPicture
drawBoard = foldl drawCell wxblank
	where
		offset = cellWidth `div` 3
		offsetM = cellWidth `div` 2
		drawCell pic ((x,y), Dead) = pic `wxPicOver` drawRect WX.black x y (cellWidth-1) (cellWidth-1)
		drawCell pic ((x,y), Alive tb ckd ns) =  nLines x y ns `wxPicOver` (smallRect x y ckd) `wxPicOver` (bigRect x y tb) `wxPicOver` pic
		smallRect x y ckd = drawRect (colourMapCKD ckd) (x + offset) (y + offset) offset offset
		bigRect x y tb = drawRect (colourMapTB tb) x y (cellWidth-1) (cellWidth-1)
		nLines x y ns = foldl wxPicOver wxblank $ map (drawLine WX.black (x+offsetM,y+offsetM) . tmap (+offsetM)) ns

{- OLD - grid layout
drawBoard :: States -> WXPicture
drawBoard = foldl drawCell wxblank
	where
		drawCell pic ((x,y), (tb, ckd)) =  (smallRect x y ckd) `wxPicOver` (bigRect x y tb) `wxPicOver` pic
		smallRect x y ckd = drawRect (colourMapCKD ckd) (x * cellWidth + cellWidth `div` 8) (y*cellWidth + cellWidth `div` 8) (cellWidth `div` 4) (cellWidth `div` 4)
		bigRect x y tb = drawRect (colourMapTB tb) (x * cellWidth) (y*cellWidth) (cellWidth-1) (cellWidth-1)
-}
		
colourMapTB :: TBState -> WX.Color
colourMapTB TBSus = WX.blue
colourMapTB TBActiveUD = WX.red
colourMapTB TBActiveD = WX.yellow
colourMapTB LITBI = WX.green


colourMapCKD :: CKDState -> WX.Color
colourMapCKD st
	| st == 1	= WX.white
	| st == 2	= WX.blue
	| st == 3	= WX.green
	| st == 4	= WX.cyan
	| st == 5	= WX.yellow
	| st == 6	= WX.magenta
	| st == 7	= WX.red
	| st == 8	= WX.black
	| otherwise	= WX.black


--the main window
mainGUI :: WXGUI () ()
mainGUI = wxVBox ( proc _ -> do
		t <- wxBoxSF localTime -< ()
		_ <- wxtext (ttext "") -< ttext ("Time: " ++ (show t))
		state <- wxBoxSF (runGame seed 0.1) -< ()
		_ <- wxpicture (psize screenSize) -< ppic (drawBoard state `wxPicOver` bg)
		returnA -< () )

--main entry point to program
main = startGUI "Epidemic 2 - Electric Boogaloo" mainGUI


--------------------------------------
--				UTIL				--
--------------------------------------

tmap :: (a -> b) -> (a,a) -> (b,b)
tmap f (a,b) = (f a, f b)


drawRect :: WX.Color -> Int -> Int -> Int -> Int -> WXPicture
drawRect col x y w h = wxWithColor col $ wxPicFill $ wxellipse box
	where box = WX.rect (WX.Point x y) (WX.sz w h)
	
drawLine :: WX.Color -> (Int, Int) -> (Int, Int) -> WXPicture
drawLine col (x,y) (x',y') = wxWithColor col $ wxPicFill $ wxline (WX.Point x y) (WX.Point x' y')

--Extension to WXFruit
wxline :: WX.Point -> WX.Point -> WXPicture
wxline pt pt' props dc _ = WX.line dc pt pt' props
  
