--fish.hs
-- a fish follows the cursor

{-# LANGUAGE Arrows #-}

module Main where

import WXFruit
import FRP.Yampa hiding (left, right, next)
import Control.Arrow hiding (left, right)
import qualified Graphics.UI.WX as WX


type Velocity 		= Double
type Angle			= Double


--THE FISH

type FishState = (WX.Point, Angle)

fishHeight, fishWidth :: Double
fishHeight = 20
fishWidth = fishHeight / 4
fishSpeed = 200

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
calcVel :: SF Angle (Velocity, Velocity)
calcVel = proc angle -> do
		let xvel = fishSpeed * (cos angle)
		let yvel = fishSpeed * (sin angle)
		returnA -< (xvel, yvel)


--State, mouse position to new state and then move
moveFish :: FishState -> SF WX.Point FishState
moveFish (pt0, theta0) = proc targetPos -> do
	rec
		newDirection <- calcDirection -< (targetPos, fishPos)
		vel <- calcVel -< fishDir
--		(velX, velY) <- calcVel -< fishDir
--		let closeEnough = (hypotenuse velX velY) <= (fromIntegral pointThreshold)
--		let vel = if not closeEnough then (velX, velY) else (0,0)
		(newX, newY) <- integral -< vel
		let fishPosNew = WX.Point (round newX) (round newY)
		fishPos <- dHold pt0 -< (Event fishPosNew)
		fishDir <- dHold theta0 -< (Event newDirection)
	returnA -< (fishPos, fishDir)
							


--draws the fish
drawFish :: SF FishState WXPicture
drawFish = proc (WX.Point x y, dir) -> do
	--CIRCLE
	--let fishArea = WX.rect (WX.Point (x - radius) (y - radius)) (WX.sz diameter diameter)
	--let fishPic = wxWithColor WX.yellow $ wxPicFill $ wxellipse fishArea
	--TRIANGLE/POLYGON
	let dir2 = pi / 2 - dir
	let m = fishHeight * (cos dir)
	let n = fishWidth * (cos dir2)
	let a = fishHeight * (sin dir)
	let b = fishWidth * (sin dir2)
	let calcPoint1 = (-(round (m+n)) + x, -(round (a-b)) + y)
	let calcPoint2 = (-(round (m-n)) + x, -(round (a+b)) + y)
	let (x1,y1) = calcPoint1
	let (x2,y2) = calcPoint2
	let fishPic = wxWithColor WX.green $ wxPicFill $ wxpoly 	[
																(WX.Point x y) 
																,(WX.Point x1 y1) 
																,(WX.Point x2 y2) 
																]
	returnA -< fishPic
	
	
--BUBBLES
bubbleSpeed :: Double
bubbleSpeed = 1

bubble :: Int -> Int -> Int -> Time -> Time -> SF WXPicture WXPicture
bubble xCentre yCentre r oscFactor tOff = proc pic -> do
	rec
		newY <- integral -< bubbleSpeed
		y <- dHold yCentre -< Event (if (y < 0) then screenDim + r else y - round bubbleSpeed)
	t <- time -< ()
--	let y = yCentre
	let x = round (sin (t + tOff) * oscFactor) + xCentre
	let bubbleArea = WX.rect (WX.Point x y) (WX.sz r r)
	let bubblePic = wxWithColor WX.white $ wxPicFill $ wxellipse bubbleArea
	returnA -< bubblePic `wxPicOver` pic

genBubbles :: SF () WXPicture
genBubbles = 	constant wxblank 
				>>> bubble 520 360 30 20 1
				>>> bubble 200 870 20 35 2
				>>> bubble 56 130 10 15 3
				>>> bubble 734 568 23 10 4
				>>> bubble 345 749 14 25 5
				>>> bubble 874 276 27 35 6
				>>> bubble 150 400 40 24 7
				
--SEAWEED
seaweedSize = 14
seaweedOsc = 6
drawSeaweed :: Int -> Int -> WXPicture
drawSeaweed x y = wxWithColor WX.cyan $ wxPicFill $ wxellipse (WX.rect (WX.Point x y) (WX.sz seaweedSize seaweedSize))

seaweedGen :: Int -> Int -> SF () WXPicture
seaweedGen x d = constant (x, screenDim) >>> seaweed d

seaweed :: Int -> SF (Int, Int) WXPicture
seaweed 0 = constant wxblank
seaweed n = proc (px,py) -> do
		t <- time -< ()
		let x = px + round ((sin (t + (fromIntegral n)/2)) * seaweedOsc)
		let y = py - seaweedSize `div` 2
		nextPic <- seaweed (n-1) -< (x,y)
		returnA -< nextPic `wxPicOver` drawSeaweed x y
		
seaweed_floor :: SF () WXPicture
seaweed_floor = proc _ -> do
			weed1 <- seaweedGen 100 15 -< ()
			weed2 <- seaweedGen 300 15 -< ()
			weed3 <- seaweedGen 500 15 -< ()
			weed4 <- seaweedGen 700 15 -< ()
			weed5 <- seaweedGen 900 15 -< ()
			returnA -< weed1 `wxPicOver` weed2 `wxPicOver` weed3 `wxPicOver` weed4 `wxPicOver` weed5
			
			
	
--MAIN GUI
screenDim = 750
screenSize = WX.sz screenDim screenDim
startPos = WX.Point 0 0
startDir = 0
fishStart = (startPos, startDir)

bg :: WXPicture
bg = wxWithColor WX.blue wxfill


fish :: WXGUI () ()
fish = wxHBox $ proc _ -> do
		mpos <- wxmouse -< ()
--		screenPic <- wxBoxSF (drSwitch drawBall) -< (mpos, drawBall)
		fishPic <- wxBoxSF (drawFish <<< (moveFish fishStart)) -< mpos
		bubblePic <- wxBoxSF genBubbles -< ()
		weeds <- wxBoxSF seaweed_floor -< ()
		_ <- wxpicture (psize screenSize) -< ppic (bubblePic `wxPicOver` weeds `wxPicOver` fishPic `wxPicOver` bg)
		returnA -< ()

--UTILITIES

hypotenuse a b = sqrt (a*a + b*b)

--WXFRUIT AUGMENTATION
wxpoly :: [WX.Point] -> WXPicture
wxpoly pts props dc _ = WX.polygon dc pts props

--MAIN PROGRAM
main :: IO ()
main = startGUI "Oli's Fantastical Fish and Wonderful Waterquarium!" fish
