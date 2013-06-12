--prisonGUI.hs

{-# LANGUAGE Arrows #-}

module Main where


import WXFruit
import FRP.Yampa hiding (left, right, next)
import FRP.Yampa.Utilities (parZ)
import Control.Arrow hiding (left, right)
import qualified Graphics.UI.WX as WX
import Data.Array
import Prisoner
import Random


--constants/dimensions
cellWidth = 15
screenDim = cellWidth * boardSize
screenSize = WX.sz screenDim screenDim
seed = 12
tick = 0.5

--main background
bg :: WXPicture
bg = wxWithColor WX.white wxfill


--draws the board
drawBoard :: StateGrid -> WXPicture
drawBoard world = foldl drawCell wxblank $ indices world
	where
		drawCell pic (x,y) = pic `wxPicOver` drawRect (colour  (x,y)) (x * cellWidth) (y*cellWidth) (cellWidth-1) (cellWidth-1)
		colour k = let (state, _, _) = world!k in colourMap $ state
		
		
colourMap :: Strat -> WX.Color
colourMap AlwaysCoop = WX.green
colourMap AlwaysDefect = WX.red
colourMap TitForTat = WX.blue
colourMap Dead = WX.black
--colourMap _ = WX.yellow --shouldn't happen

--the main window
mainGUI :: StdGen -> WXGUI () ()
mainGUI g = wxVBox ( proc _ -> do
		state <- wxBoxSF (runGame g tick) -< ()
		_ <- wxpicture (psize screenSize) -< ppic (drawBoard state `wxPicOver` bg)
		returnA -< () )

--main entry point to program
main = startGUI "Ay, There's the Rub" (mainGUI (mkStdGen seed))



--------------------------------------
--				UTIL				--
--------------------------------------

drawRect :: WX.Color -> Int -> Int -> Int -> Int -> WXPicture
drawRect col x y w h = wxWithColor col $ wxPicFill $ wxrect box
	where box = WX.rect (WX.Point x y) (WX.sz w h)