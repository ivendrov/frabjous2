{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Main where

import Control.Monad hiding (when)
import Control.Monad.Identity (Identity)
import Control.Arrow
import Control.Wire
import System.Random (mkStdGen)
import Text.Printf
import Prelude hiding ( (.), id)
import Data.List as List
import Data.Vector as Vector
import Data.Ord
import Data.Monoid

import Control.Wire.Classes

data State = Susceptible | Infectious | Recovered deriving (Eq, Show)


-- rate generates events according to a poisson process with parameter 1/mu
-- problem : what about multiple occurrences in a single time interval?? 
rate :: (Monoid e, RandomGen g) => Time -> g -> Event e m a
rate mu g
     | mu <= 0 =  error "mean time cannot be negative"
     | otherwise = mkPure $
       \dt x -> 
        let (e,g') = random g in
        (if (e < 1 - exp (-dt / mu)) then Right x else Left mempty, rate mu g')
  
rate' mu = rate mu (mkStdGen 3)

startingInfected = [False, False, False, False]


-- STATECHART specified by a state to wire function,
-- and a "status" wire that does the plumbing to ensure wires are reset, etc

--never = Control.Wire.empty -- event that never triggers 

stateToWire :: State -> WireP () State
stateToWire state =
    transitions <|> pure state where
        transitions = 
            case state of 
              Susceptible -> pure Infectious . rate' 2
              Infectious -> pure Recovered . after 3
              Recovered -> pure Susceptible . after 3



status :: WireP () State
status = rSwitch stateToWire Susceptible

-- rSwitch switches between wires based on the given discriminator function "computeWire"
-- currently, every time its current wire changes output, it plugs the output into computeWire
-- to determine the wire to switch to.
-- TODO: decompose the "every time wire changes" and the "select wire based on a function" 
--       into two separate combinators. The second combinator could be used to implement
--       phase shifts in other variables (e.g. income calculation as child vs adult)
rSwitch computeWire initialState = helper' initialState (stateToWire initialState) where
    helper' state currentWire = 
        mkGen $ \dt x -> do
          (output, currentWire') <- stepWire currentWire dt x
          case output of 
            Left _ -> error "status wire should never inhibit"
            Right newState -> 
                return (output, 
                        helper' 
                          newState           
                          (if (newState == state) then currentWire' else stateToWire newState))
           

{-
info :: WireP () Bool
info = proc _ -> do
         infected <- delay False . id -< prevInfected
         if infected
         then do
           prevInfected <- id -< infected
           returnA -< prevInfected
         else do
           prevInfected <- arr not -< infected
           returnA -< prevInfected
         

-}




wire :: WireP () String
wire = forI 100 . arr show . status

control whenInhibited whenProduced wire = loop wire (counterSession 0.2) where
    loop w' session' = do
      (mx, w, session) <- stepSessionP w' session' ()
      case mx of 
        Left ex -> whenInhibited ex
        Right x -> do whenProduced x
                      loop w session
main = control return (putStrLn) $ wire

{-
info = proc _ -> do 
   rec incomes <- delay startIncomes . arr calcIncomes -< (nbhdNetwork, avgNbhdIncomes)
       nbhdNetwork <- delay startingNbhdNetwork . arr movePeople -< (nbhdNetwork, incomes, avgNbhdIncomes)
       avgNbhdIncomes <-  arr calcAverages -<  (nbhdNetwork, incomes)
       healths <- arr calcHealths -< incomes
       timer <- time -< ()
   returnA -< (timer, (adjList1 nbhdNetwork), avgNbhdIncomes, incomes)
-}
{-
system =
    proc _ -> do 
      rec c1 <- countFrom 10 -< 1
      if even c1
          then returnA -< 0
          else do
              c2 <- countFrom 20 -< 1
              returnA -< (c1 :: Int) * 100000 + (c2 :: Int)
-}