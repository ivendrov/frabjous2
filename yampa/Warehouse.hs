--warehouse.hs
--
-- an example program simulating a warehouse alarmsystem
-- current system handles a single door, a single window, the internal temperature
-- and doesn't handle simultaneous events

{-# LANGUAGE Arrows #-}

module Warehouse where

import FRP.Yampa
import FRP.Yampa.Event



--data definition
type Temperature = Int


data Sensing =	  DoorOpened
				| DoorClosed
				| WindowOpened
				| WindowClosed
--				| TempChangedTo Temperature
--				| TempChangedBy Temperature

type WInput = (Temperature, Event Sensing)


--constants
tempCutoff :: Temperature
tempCutoff = 40

tempDefault :: Temperature
tempDefault = 30

--basic monitoring system

--interpreting basic sensings
--door
doorOpen :: SF WInput Bool
doorOpen = arr snd >>> arr (mapFilterE doorMap) >>> hold False

doorMap :: Sensing -> Maybe Bool
doorMap DoorOpened 	= Just True
doorMap DoorClosed	= Just False
doorMap _			= Nothing

--window
winOpen :: SF WInput Bool
winOpen = arr snd >>> arr (mapFilterE winMap) >>> hold False

winMap :: Sensing -> Maybe Bool
winMap WindowOpened = Just True
winMap WindowClosed = Just False
winMap _			= Nothing

--temperature
tempSF :: SF WInput Temperature 
tempSF = arr fst >>> eventifyTemp >>> hold tempDefault

eventifyTemp :: SF Temperature (Event Temperature)
eventifyTemp = proc x -> do
				returnA -< Event x

--monitors that react to the basic sensors
tempMonitor :: SF WInput (Event String)
tempMonitor = tempSF >>> arr (> tempCutoff) >>> edge >>> arr (`tag` "Temperature is high")


doorMonitor :: SF WInput (Event String)
doorMonitor = doorOpen >>> edge >>> arr (`tag` "Door Opened")

windowMonitor :: SF WInput (Event String)
windowMonitor = winOpen >>> edge >>> arr (`tag` "Window Opened")

alarm :: SF WInput String 
alarm = proc inp -> do
			dMon <- doorMonitor -< inp
			wMon <- windowMonitor -< inp
			tMon <- tempMonitor -< inp
--			val <- arr (`tag` (++)) >>> accumHold [] -< dMon `lMerge` wMon
			let
				result NoEvent 	= ""
				result x		= fromEvent x
			returnA -< result (dMon `lMerge` wMon `lMerge` tMon)


{-

-- TEST 1 -------------------------------------------------------

--the goings-on of test 1
test1Events :: SF a WInput
test1Events = afterEach [	(2.0, DoorOpened)
							,(1.0, WindowOpened)
							,(1.0, DoorClosed)
							,(1.0, TempChangedTo 35)
							,(1.0, WindowClosed)
							,(2.0, TempChangedTo 42)
							,(1.0, TempChangedTo 45)
							,(0.5, DoorOpened)
							,(1.5, TempChangedTo 38)
							,(1.5, TempChangedTo 32)
							,(2, DoorClosed)
						]
							
-- relative time ticking
test1Rate = 1

--run this to run the test
--test1 t_max = [y | (x,y) <- takeWhile ((<= t_max) . fst) tios]
test1 t_max = filter ( (/= "") . snd) ( takeWhile ((<= t_max) . fst) tios)
	where
		--time, input, output
		tios = embed (localTime &&& (test1Events >>> alarm)) (deltaEncode test1Rate (repeat()))
-}
