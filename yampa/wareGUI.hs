--wareGUI.hs
--
--warehouse sim with a GUI (via wxFruit)

{-# LANGUAGE Arrows #-}

module Main where

import WXFruit
import FRP.Yampa hiding (left, right, next)
import FRP.Yampa.Event
import Control.Arrow hiding (left, right)
import qualified Graphics.UI.WX as WX
import Warehouse

safeText = "All is quiet"
alarmText = "DANGER WILL ROBINSON!"
alarmTime :: Time
alarmTime = 2

wareCtrl :: WXGUI () WInput
wareCtrl = wxHBox ( proc _ -> do
		rec
			d <- wxBoxSF doorOpen -< (temp, inputE)
			w <- wxBoxSF winOpen -< (temp, inputE)
			t <- wxBoxSF tempSF -< (temp, inputE)
			opD <- wxbutton (btext "Open Door") -< benabled (not d)
			clD <- wxbutton (btext "Close Door") -< benabled d
			opW <- wxbutton (btext "Open Window") -< benabled (not w)
			clW <- wxbutton (btext "Close Window") -< benabled w
			temp <- wxslider Horiz 10 50 (sselection tempDefault) -< sselection t
			inputE <- wxBoxSF (dHold noEvent) -< (merge (tag opD (Event DoorOpened))
													(merge (tag opW (Event WindowOpened))
															(merge (tag clD (Event DoorClosed))
																(tag clW (Event WindowClosed)))))
		returnA -< (temp, inputE) )

wareGUI :: WXGUI () ()
wareGUI = wxVBox ( proc _ -> do
		input <- wxBox wareCtrl -< ()
		text <- wxBoxSF alarm -< input
		reset <- wxBoxSF shouldReset -< text
		dispTxt <- wxBoxSF (dHold safeText) -< if reset then Event safeText
											else (if text == "" then noEvent else Event alarmText)
		wxtext (ttext "foo") -< (ttext dispTxt)
		returnA -< () )
	
shouldReset :: SF String Bool	
shouldReset = arr (== "") >>> edge >>> delayEvent alarmTime >>> arr isEvent
			
main :: IO ()
main = startGUI "Warehouse Monitor" wareGUI