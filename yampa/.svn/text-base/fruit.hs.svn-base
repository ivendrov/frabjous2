--fruit.hs
--
-- an example program for testing out wxfruit

{-# LANGUAGE Arrows #-}


--imports lifted from "paddle.hs", the example for wxFruit
import WXFruit
import FRP.Yampa hiding (left, right, next)
import Control.Arrow hiding (left, right)
import qualified Graphics.UI.WX as WX

{-
ballGUI :: SimpleGUI
ballGUI = proc gin -> do
	gin 	>- mouseSF 			-> mouse
	mouse 	>- liftSF moveBall 	-> bpic
	bpic 	>- returnA
-}




--lifted from Functionally Modeled User Interfaces; adapted for wxFruit
data MediaState = Playing | Paused | Stopped
	deriving(Eq, Show)

playerCtrl :: WXGUI () MediaState
playerCtrl = wxHBox ( proc _ -> do
		rec
			playE <- wxbutton (btext "Play" ) -< benabled (state /= Playing)
			pauseE <- wxbutton (btext "Pause") -< benabled (state == Playing)
			stopE <- wxbutton (btext "Stop") -< benabled (state /= Stopped)
			state <- wxBoxSF (dHold Stopped) -< (merge (tag playE Playing) (merge (tag pauseE Paused) (tag stopE Stopped)))  --dHold lets recursion work 
		returnA -< state)

player :: WXGUI () ()
player = wxVBox ( proc _ -> do
	state <- wxBox playerCtrl -< ()
	wxtext (ttext "State: Stopped") -< (ttext ("State: " ++ (show state)))  
	returnA -< () )


main :: IO ()
main = startGUI "player" player




{-
      let ballS = WX.rect (WX.point (round (xpos - 12.5)) (round (ypos - 12.5)))
                          (WX.sz 25 25)
      let ballPicS = wxWithColor WX.yellow $ wxPicFill $ wxellipse ballS
      -}