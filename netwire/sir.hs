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
import Data.Function (on)
import Data.Graph
import Data.Tuple (swap)

import Control.Wire.Classes

-- GENERAL NETWORK STUFF (should work both for 2D (DPH) and 1D (GPU) network representations)
data Network = Network {adjList :: Vector (Vector Int)} deriving Show

foldAdjList :: Network -> (Vector a -> b) -> Vector a -> Vector b 
-- foldAdjList network agents adjFold returns a vector whose ith element
-- is adjFold applied to all the neighbours of the ith agent
foldAdjList network adjFold agents = Vector.map (adjFold . backpermute agents) (adjList network)


-- SIR combinators / data 

data State = S | I | R deriving (Eq, Show)


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

-- a wire that takes as input the rate of infection, and produces a unit value with a rate
-- corresponding to the given time, minus inaccuracy with multiple occurrences in a 
-- single time interval
rateWire ::  (RandomGen g) => g -> WireP Double ()
rateWire g = mkPure $
           \dt lambda -> 
               let (e, g') = random g in
               (if (e < 1 - exp (-dt * lambda)) then Right () else Left mempty, rateWire g')

rateWire' = rateWire (mkStdGen 3)
               




-- STATECHART specified by a state to wire function,
-- and a "status" wire that does the plumbing to ensure wires are reset, etc
infectionRatePerPerson = 0.5
calcInfectionRate :: Vector State -> Double
calcInfectionRate = (*infectionRatePerPerson) . 
                    fromIntegral . 
                    Vector.length . 
                    Vector.filter (==I)

--never = Control.Wire.empty -- event that never triggers 
type Cell = WireP Double State

stateToWire :: State -> Cell
stateToWire state =
    transitions <|> pure state where
        transitions = 
            case state of 
              S -> pure I . rateWire' 
              I -> pure R . after 3
              R -> pure S . after 3



genCell :: State -> Cell
genCell = rSwitch stateToWire

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

-- Parallel wire operator (just pure wires for now)
par :: Vector (WireP a b) -> WireP (Vector a) (Vector b)
par wires = 
    mkPure $ \dt inputs -> 
        let dts = Vector.replicate (Vector.length wires) dt 
            (values, wires') = Vector.unzip $ Vector.zipWith3 stepWireP wires dts inputs 
            right x = case x of 
                        Right y -> y
                        Left _ -> error "cannot extract right from a Left contructor"
            isRight x = case x of
                          Right _ -> True
                          Left _ -> False
            value = if (Vector.all isRight values) 
                    then Right $ Vector.map right values
                    else Left mempty -- if a single wire doesn't produce, nothing produces
        in (value, par wires')
 
           
-- THIS SPECIFIC EXAMPLE
randomNetwork :: RandomGen r => r -> Double -> Int -> Network
-- randomNetwork rng fraction size = a random network with the given number of nodes. 
--   each pair of nodes has a "fraction" probability of an edge

randomNetwork rng fraction size = Network $ fromList . List.map (fromList) $ adjList where
    edges' = List.map snd . List.filter ((<fraction) . fst) . List.zip (randoms rng) $ 
            [(u,v) | u <- [0 .. size - 1], 
                     v <- [u+1 .. size -1]]
    edges = edges' List.++ (List.map swap edges')
    adjList = List.map (\i -> List.map (snd) . List.filter (\edge -> fst edge == i) $ edges) [0 .. size -1] 



size = 10
fraction = 0.3
sirNetwork =  randomNetwork (mkStdGen 32498394823) fraction size
startingStates = Vector.replicate size S // [(0, I)]

sirWire :: WireP (Vector State) (Vector State)
sirWire = par (Vector.map genCell startingStates) . 
          arr (foldAdjList sirNetwork calcInfectionRate)


sir :: WireP () (Vector State)

-- have to write own combinator because arrow doesn't (probably) satisfy ArrowLoop
sir = helper startingStates sirWire
                             where 
    helper states wire = mkPure $ \dt _ ->                         
                         let (Right states', wire') = stepWireP wire dt states
                         in (Right states' , helper states' wire')
                         
      
     
        
test :: WireP () Bool
test = proc _ -> do
         rec a <- delay True . arr (not) . arr (==False)  -< a
         returnA -< a

wire :: WireP () String
wire = forI 150 . arr show . sir

control whenInhibited whenProduced wire = loop wire (counterSession 0.2) where
    loop w' session' = do
      (mx, w, session) <- stepSessionP w' session' ()
      case mx of 
        Left ex -> whenInhibited ex
        Right x -> do whenProduced x
                      loop w session
main = do 
  putStrLn . show $ sirNetwork 
  control return (putStrLn) $ wire
