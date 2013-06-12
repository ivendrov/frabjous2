-- reactimate.hs
-- 
-- a test for using reactimate


{-# LANGUAGE Arrows #-}

import FRP.Yampa
import FRP.Yampa.Event
import Graphics.UI.WX

--constants
circleRadius = 10

test :: IO ()
test = reactimate rInit sense actuate sf



rInit :: IO (Point)
rInit = do {start setupWindow; return (0,0)}


--placeholder
sense :: Bool-> IO (DTime, Maybe Point)
sense True = return (0.1, Nothing)
sense False = return (0.1, Nothing)

actuate :: Bool -> Point -> IO Bool
actuate b pt = do
			drawCircle pt
			return False

sf :: SF Point Point
sf = hold (0,0)



--IO stuff

setupWindow :: IO ()
setupWindow  = do
		-- non-user resizable top-level frame
		f <- frameFixed [text := "Bouncing balls"]
		--panel to draw in
		p <- panel f []	
		set f [layout := margin 10 (floatCentre (widget p))]
		
drawCircle :: Point -> IO ()
drawCircle pt = do
	circle (DC [brushColor := red, brushKind := BrushSolid]) pt circleRadius []