-- wolf.hs
--
-- visual simulation of lotka-volterra equations

{-# LANGUAGE Arrows #-}

import WXFruit
import FRP.Yampa hiding (left, right, next)
import Control.Arrow hiding (left, right)
import qualified Graphics.UI.WX as WX
import LotkaVolterra

--GUI
screenDim = 1200
screenSize = WX.sz screenDim screenDim
initPred = 5
initPrey = 28
yOffset = screenDim - rh `div` 2
yScale = 30
preyX = screenDim `div` 4
predX = screenDim `div` 4 * 3
rw = 100
rh = 50

bg :: WXPicture
bg = wxWithColor WX.white wxfill



drawRect :: WX.Color -> Int -> Int -> WXPicture
drawRect col x y = wxWithColor col $ wxPicFill $ wxrect box
	where box = WX.rect (WX.Point x y) (WX.sz rw rh)
	
drawPred :: Population -> WXPicture
drawPred = (drawRect WX.red predX) . (yOffset -) . (yScale *)

drawPrey :: Population -> WXPicture
drawPrey = (drawRect WX.green preyX) . (yOffset -) . (yScale *)

wolf :: WXGUI () ()
wolf = wxVBox ( proc _ -> do
	(pred, prey) <- wxBoxSF (nextStep initPred initPrey) -< ()
	let predPic = drawPred pred
	let preyPic = drawPrey prey
	wxtext (ttext "prey") -< (ttext ("prey: " ++ (show prey)))
	wxtext (ttext "pred") -< (ttext ("pred: " ++ (show pred)))
	_ <- wxpicture (psize screenSize) -< ppic (preyPic `wxPicOver` predPic `wxPicOver` bg)
	returnA -< () )


--MAIN PROGRAM
main :: IO ()
main = startGUI "The Lovely and Voracious Lotka-Volterra Equations" wolf