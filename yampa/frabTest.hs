--frabTest.hs


{-# LANGUAGE Arrows #-}

module Main where


import WXFruit
import Frabjous
import FRP.Yampa hiding (left, right, next)
import Control.Arrow hiding (left, right)
import qualified Graphics.UI.WX as WX



--the main window
mainGUI :: WXGUI () ()
mainGUI = wxVBox ( proc _ -> do
		e <- wxBoxSF timeoutDF -< (2, "after 5")
		str <- wxBoxSF $ hold "going" -< e
		_ <- wxtext (ttext "start") -< ttext str
		returnA -< () )

--main entry point to program
main = startGUI "Test" mainGUI