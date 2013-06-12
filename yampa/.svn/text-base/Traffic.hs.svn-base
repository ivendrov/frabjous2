-- Traffic.hs
--
-- a traffic light simulator written w/ Yampa

{-# LANGUAGE Arrows #-}

module Traffic where

import FRP.Yampa


--DATA DEFINITIONS-----------------------------------

--general movement
type Distance = Double
type Velocity = Double
type Acceleration = Double

--lights
data LightState = Green | Amber | Red
	deriving(Show, Eq, Enum)

type LightDelay = (Time, Time, Time) --time for Green, Amber, Red, respectively
type Light = (Distance, LightState)
type Lights = [Light]

--car
data CarState = Accelerating | Cruising | Slowing | Stopped
	deriving(Show, Eq, Enum)
type Car = (CarState, Velocity, Distance)

--constants
carAcc = 2
carBraking = 3


--SIGNAL FUNCTIONS-----------------------------------

--car
controlCar :: SF (Car, Lights) Car
moveCar :: SF Car Car
shouldCarSlow :: Distance -> Lights -> Bool --super basic AI

--lights

moveLights :: Lights -> SF () Lights
moveLights [] = never 
moveLights [(d, s):ls] = cur `catEvents` moveLights ls 
		where
			cur = repeatedly t changeLight >>> accumHold Green
			t = 1 --PLACEHOLDER


changeLight :: LightState -> LightState
changeLight c
		| c == Red	= Green
		| otherwise = succ c

--MAIN PROGRAM/UTILS---------------------------------

--main simulation driver 
runSim :: Lights -> SF (Car, Lights)

