-- multi.hs
--
-- a quick test for multiple agents
-- simulates a red light/green light game

{-# LANGUAGE Arrows #-}

import WXFruit
import FRP.Yampa hiding (left, right, next)
import Control.Arrow hiding (left, right)
import qualified Graphics.UI.WX as WX

screenDim, diameter :: Int
screenDim = 750
diameter = 50

screenSize = WX.sz screenDim screenDim

type Position = Int
type Acceleration = Double

moveAgent :: Position -> Acceleration -> SF Bool (Position, Position)
moveAgent x a = proc b -> do
		v <- integral -< if b then a else 0
		y <- integral -< if b then v else 0
		returnA -< (x, round y)
		
		
drawAgent :: SF (Position, Position) WXPicture
drawAgent = proc (x, y) -> do
		let area = WX.rect (WX.Point x y) (WX.sz diameter diameter)
		let pic = wxWithColor WX.yellow $ wxPicFill $ wxrect area
		returnA -< pic
		
runAgent x a = moveAgent x a >>> drawAgent
	
collapsePics :: [WXPicture] -> WXPicture
collapsePics = foldr wxPicOver wxblank

myAgents = [runAgent 50 20, runAgent 400 10, runAgent 700 5]

bg = wxWithColor WX.white wxfill

goButton :: WXGUI () Bool
goButton = wxHBox (proc _ -> do
		rec
			e <- wxbutton (btext "click me") -< benabled True
			b <- wxBoxSF (dHold False) -< e `tag` (not b)
		returnA -< b )

mainGUI :: WXGUI () ()
mainGUI = wxVBox (proc _ -> do
	b <- wxBox goButton -< ()
	pic <- wxBoxSF (parB myAgents >>> arr collapsePics) -< b
	_ <- wxpicture (psize screenSize) -< ppic (pic `wxPicOver` bg)
	returnA -< ())

main :: IO ()
main = startGUI "Multi Agent Test" mainGUI