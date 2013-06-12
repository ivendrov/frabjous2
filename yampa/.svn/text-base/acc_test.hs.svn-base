-- acc_test.hs
--
-- Acceleration, distance, etc. etc.

{-# LANGUAGE Arrows #-}

import FRP.Yampa

-- relative time ticking
smplPer = 1

type Acceleration = Double
type Velocity = Double
type Distance = Double

--main entry point
main :: IO ()
main = do 
	putStr "Constant Acceleration Simulation\n"
	putStr "(Acceleration, Velocity, Distance)\n"
	putStr "Initial Acceleration? "
	initAcc <- getLine
	putStr "Amount of time to simulate? "
	simTime <- getLine
	putStr ((show (getFirst (read simTime :: Int) (read initAcc :: Acceleration) )) ++ "\n")
	
	

--definition of motion
--acc :: SF a Acceleration
--acc = constant 1.2

vel :: SF Acceleration Velocity
vel = proc a -> do
	v <- integral -< a
	returnA -< v
	
dist :: SF Velocity Distance
dist = proc v -> do
	d <- integral -< v
	returnA -< d

--calculate acceleration, velocity and distance given an acceleration
formatTriple :: SF Acceleration (Acceleration, Velocity, Distance)
formatTriple = proc a -> do
		v <- vel -< a
		d <- dist -< v
		returnA -< (a,v,d)

--get first n acc/vel/dist triples of simulation
getFirst :: Int -> Acceleration -> [(Acceleration, Velocity, Distance)]
getFirst n k = take n (map snd tios)
	where
		tios = embed (localTime &&& ( (constant k) >>> formatTriple)) (deltaEncode smplPer (repeat ()) )