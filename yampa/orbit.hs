--fish.hs
-- a fish follows the cursor

{-# LANGUAGE Arrows #-}

module Main where

import WXFruit
import FRP.Yampa hiding (left, right, next)
import Control.Arrow hiding (left, right)
import qualified Graphics.UI.WX as WX


type Acceleration	= Double
type Velocity 		= Double
type Angle			= Double


--THE FISH

type FishState = (WX.Point, Angle)

diameter = 2 * radius
radius = 100
abs_acc = 200
maxFishSpeed = 200
pointThreshold = 100

--not very good, plenty of error blowup
calcDirection :: SF (WX.Point, WX.Point) Angle
calcDirection = proc ( (WX.Point target_x target_y), (WX.Point pos_x pos_y) ) -> do
	let y2 = fromIntegral target_y
	let y1 = fromIntegral pos_y
	let x2 = fromIntegral target_x
	let x1 = fromIntegral pos_x
	let ratio = (y2 - y1) / (x2 - x1)
	returnA -< if x2 >= x1 then atan ratio else pi + (atan ratio)
	
	
--calculates the partial velocities based on direction
calcAcc :: SF Angle (Acceleration, Acceleration)
calcAcc = proc angle -> do
		let xacc = abs_acc * (cos angle)
		let yacc = abs_acc * (sin angle)
		returnA -< (xacc, yacc)


hypotenuse a b = sqrt (a*a + b*b)

--REMOVE THIS--------------------------------------------------------------
--terrible clamping
clampVel (v1, v2) = if hypot < maxFishSpeed then (v1, v2) else scaleDown
	where
		hypot = hypotenuse v1 v2
		scalefactor = (maxFishSpeed / hypot) 
		scaleDown = (scalefactor * v1, scalefactor *v2)		
---------------------------------------------------------------------------




--State, mouse position to new state and then move
moveFish :: FishState -> SF WX.Point FishState
moveFish (pt0, theta0) = proc targetPos -> do
	rec
		newDirection <- calcDirection -< (targetPos, fishPos)
		acceleration <- calcAcc -< fishDir
		vel <- integral -< acceleration
{-		let target = (fromIntegral m, fromIntegral n)
			where (WX.Point m n) = targetPos
		let pos = (fromIntegral m, fromIntegral n)
			where (WX.Point m n) = fishPos
		let closeEnough (a,b) (c,d) = (hypotenuse (c - a) (d - b)) <= pointThreshold
		(newX, newY) <- integral -< if not (closeEnough target pos)
									then clampVel vel
									else (0,0)
-}
		(newX, newY) <- integral -< clampVel vel
		fishPosNew <- identity -< WX.Point (round newX) (round newY)
		fishPos <- dHold pt0 -< (Event fishPosNew)
		fishDir <- dHold theta0 -< (Event newDirection)
	returnA -< (fishPos, fishDir)
							


--draws the fish
drawFish :: SF FishState WXPicture
drawFish = proc (WX.Point x y, _) -> do
	let fishArea = WX.rect (WX.Point (x - radius) (y - radius)) (WX.sz diameter diameter)
	let fishPic = wxWithColor WX.yellow $ wxPicFill $ wxellipse fishArea
	--let screenPic = ballPic `wxPicOver` bg
	returnA -< fishPic
	
	
	
	
--THE SCREEN
screenSize = WX.sz 1000 1000
startPos = WX.Point 500 500
startDir = 0
fishStart = (startPos, startDir)

--lifted from paddle.hs (example in wxFruit)
bg :: WXPicture
bg = wxWithColor WX.blue wxfill

	
fish :: WXGUI () ()
fish = wxHBox $ proc _ -> do
		mpos <- wxmouse -< ()
--		screenPic <- wxBoxSF (drSwitch drawBall) -< (mpos, drawBall)
		fishPic <- wxBoxSF (drawFish <<< (moveFish fishStart)) -< mpos
		_ <- wxpicture (psize screenSize) -< ppic (fishPic `wxPicOver` bg)
		returnA -< ()







--MAIN PROGRAM
main :: IO ()
main = startGUI "Oli's Fantastical Fish and Wonderful Waterquarium!" fish
