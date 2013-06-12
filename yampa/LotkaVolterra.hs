--lotka-volterra.hs
--
-- an example program simulating predator/prey relationships

{-# LANGUAGE Arrows #-}

module LotkaVolterra (Population, nextStep) where

import FRP.Yampa
import FRP.Yampa.Event

--type declaration
type Population = Int

--parameters
alpha 	= 1 -- growth of prey
beta 	= 0.1 -- predation
gamma 	= 0.5 -- natural death of predators
delta 	= 0.2 -- growth of predators


predCalc :: SF (Population, Population) Population
predCalc = proc (pred, prey) -> do 
				let pd = fromIntegral pred :: Double
				let py = fromIntegral prey :: Double
				d <- integral -< (-(gamma * pd) + (delta * pd * py))
				returnA -< round d

preyCalc :: SF (Population, Population) Population
preyCalc = proc (pred, prey) -> do
				let pd = fromIntegral pred :: Double
				let py = fromIntegral prey :: Double
				d <- integral -< (alpha * py) - (beta * pd * py)	
				returnA -< round d


nextStep :: Population -> Population -> SF () (Population, Population)
nextStep pred0 prey0 = proc () -> do
	rec
		newPred <- pred0 --> predCalc -< (pred, prey)
		newPrey <- prey0 --> preyCalc -< (pred, prey)
		pred <- dHold pred0 -< Event newPred
		prey <- dHold prey0 -< Event newPrey
	returnA -< (pred, prey)
			

--TESTING

-- relative time ticking
smplPer = 0.0001


--main entry point
main :: IO ()
main = do 
	putStr "\n\n\n=========================\n"
	putStr       "Lotka-Volterra Simulation\n"
	putStr       "(Predators, Prey)\n"
	putStr       "=========================\n\n"
	putStr "Initial number of predators? "
	iPred <- getLine
	putStr "Initial number of prey? "
	iPrey <- getLine
	putStr ((show ((read iPred :: Population) ,(read iPrey :: Population))) ++ "\n")
	putStr "Amount of time to simulate? "
	simTime <- getLine
	putStr ((show (runSim (read simTime :: Int) (read iPred :: Population) (read iPrey :: Population) )) ++ "\n")

--get first n outputs of simulation
runSim :: Int -> Population -> Population -> [(Population, Population)]
runSim n pred0 prey0  = take n (map snd tios)
	where
		tios = embed (localTime &&& (nextStep pred0 prey0)) (deltaEncode smplPer (repeat ()) )
