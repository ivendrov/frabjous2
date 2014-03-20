-----------------------------------------------------------------------------
-- |
-- Module      :  StandardLibrary
-- Copyright   :  (c) University of Saskatchewan 2013
-- 
-- Maintainer  :  ivan.vendrov@usask.ca
--
-- The Frabjous standard library
--------------------------------------------------------------------------
module StandardLibrary 
 (-- RANDOM NUMBERS
  uniform,
  frequencies,
  draw,
  -- WIRE COMBINATORS
  -- | simple combinators
  constant,
  function, 
  -- | event combinators
  never,
  after,
  for,
  delay,
  -- | accumulation & real numbers
  integrate,
  accumulate,
  randomWalk,
  clip,

  -- | randomness
  noise,
  rate,
  rSwitch,
  statechart,
  -- NETWORKS
  randomNetwork,
  randomSymmetricNetwork,
  poissonRandomSymmetric,
  distanceBased,
  euclidean)
where

import InternalLibrary
import Prelude hiding ((.), id)
import Control.Monad.Random
import Data.Traversable as Traversable hiding (for)
import Control.Wire (arr, mkGen, mkPure, mkState, (.), Wire, LastException, stepWire, (<|>), after, delay, for)
import qualified Control.Wire as Wire
import Data.Monoid
import Data.Tuple (swap)
import Data.List
import qualified Data.IntMap as IntMap


-- 0. RANDOM NUMBERS 

uniform :: MonadRandom m => (Double, Double) -> m Double
uniform = getRandomR 

frequencies :: MonadRandom m => [(a, Double)] -> m a
frequencies probs = 
    let (vals, pdf) = unzip probs
        cdf = scanl1 (+) pdf
    in do
      p <- getRandom
      let Just idx = findIndex (>= p) cdf
      return (vals !! idx)

draw :: MonadRandom m => Int -> m a -> m [a]
draw n rand = Traversable.sequence (replicate n rand)
      
  
  
  


-- 1. WIRE COMBINATORS

constant :: (Monad m) => b -> Wire e m a b
constant = Wire.pure 

function :: (Monad m) => (a -> b) -> Wire e m a b
function = Wire.arr

never :: (Monad m, Monoid e) => Wire e m a b
never = Wire.empty


-- | draws a value from the given distribution at every timestep
noise :: MonadRandom m => m a -> Wire LastException m input a
noise distribution = mkGen $ \_ _ -> do
                       val <- distribution
                       return (Right val, noise distribution)

-- | accumulates the output of a signal function by the given combining function, with the given starting state
accumulate :: Monad m => (b -> a -> b) -> b -> Wire e m c a -> Wire e m c b
accumulate binop init wire = Wire.hold init (Wire.accum binop init . wire)     

-- | integrates its input with respect to time
integrate :: Monad m => Double -> Wire e m Double Double
integrate = internalState (\dt x -> (+ dt*x))

-- | performs a 1D random walk at the velocity specified by the given distribution
randomWalk init distribution = integrate init . noise distribution

-- | (TODO REFACTOR?) internalState applies the given transition function
-- | to its state at every step, then outputs the state
internalState transition init = mkState init $ \dt (x, state) -> 
                                 let newState = transition dt x state
                                 in newState `seq` (Right newState, newState)

-- | the function 'clip' keeps its value in the given closed interval
clip (a,b) x = if x < a then a
               else if x > b then b
               else x


-- a wire that takes as input a rate, and produces a unit value with a rate
-- corresponding to the given time, minus inaccuracy with multiple occurrences in a 
-- single time interval
rate :: MonadRandom m  => Wire LastException m Double ()
rate = mkGen $
           \dt lambda -> do 
             e <- getRandom
             return (if (e < 1 - exp (-dt * lambda)) then Right () else Left mempty, rate)

-- rSwitch switches between wires based on the given discriminator function "computeWire"
-- currently, every time its current wire changes output, it plugs the output into computeWire
-- to determine the wire to switch to.
-- TODO: decompose the "every time wire changes" and the "select wire based on a function" 
--       into two separate combinators. The second combinator could be used to implement
--       phase shifts in other variables (e.g. income calculation as child vs adult)
rSwitch computeWire initialState = helper' initialState (computeWire initialState) where 
    helper' state currentWire = 
            mkGen $ \dt x -> do
              (output, currentWire') <- stepWire currentWire dt x
              case output of 
                Left _ -> error "status wire should never inhibit"
                Right newState -> 
                    return (output, 
                            helper'  newState           
                            (if (newState == state) then currentWire' else computeWire newState))


-- statechart :: Wire a b -> Wire a b -> Wire a b
-- statechart start transitions is a wire whose internal state will be the most recent
-- value produced by transitions; and which is refreshed every time its internal state changes
statechart start transitions = 
    mkGen $ \dt x -> do 
      (Right init, _) <- stepWire start dt x
      stepWire (rSwitch computeWire init) dt x where
                     computeWire state = transitions state <|> constant state




-- 2. NETWORKS

--  A) Library functions for creation 
pairs xs = [(u, v) | u <- xs, v <- xs, u < v]


randomNetwork, randomSymmetricNetwork :: (MonadRandom m) => Double -> [Int] -> [Int] -> m ManyToMany
-- randomNetwork rng fraction size = a random network with the given integer vertices. 
-- each directed edge has a given probability of existing

randomNetwork fraction vertices1 vertices2 = do 
    randoms <- getRandoms 
    let edges = map snd . filter ((<fraction) . fst) . zip randoms $ 
            [(u,v) | u <- vertices1, 
                     v <- vertices2]
    return $ fromEdges vertices1 vertices2 edges

-- randomSymmetricNetwork is like a random network but for a single population 
-- each UNDIRECTED edge has a "fraction" probability of existing
randomSymmetricNetwork fraction vertices _ = do
  randoms <- getRandoms
  let edges' = map snd . filter ((<fraction) . fst) . zip randoms $ 
               [(u,v) | u <- vertices, 
                v <- vertices, u < v]
      edges = edges' ++ map swap edges'
  return $ fromEdges vertices vertices edges

--  B) dynamic networks
poissonRandomSymmetric :: Double ->
                          (model -> ReactiveOutput a) ->                 
                              ModelWire model ManyToMany
poissonRandomSymmetric prob extractPop = helper where
  helper = mkGen $ \dt model -> do
             let v1 = indices . extractPop $ model
             network <- randomSymmetricNetwork prob v1 v1
             return (Right network, helper)

distanceBased :: (a -> a -> Double) -> 
                 Double -> 
                (model -> ReactiveOutput a) -> 
                    ModelWire model ManyToMany
distanceBased d limit extractPop = function helper where
    helper model = let pop = collection . extractPop $ model
                       indices = IntMap.keys pop
                       indexPairs = pairs indices
                       withinDistance (i1, i2) = d (pop IntMap.! i1) (pop IntMap.! i2) < limit
                   in fromEdges indices indices (filter withinDistance indexPairs)

euclidean :: [a -> Double] -> a -> a -> Double
euclidean accessors p1 p2 = 
    let coords1 = map ($ p1) accessors
        coords2 = map ($ p2) accessors
        diffs = zipWith (-) coords1 coords2
        sumSquares = sum $ map (^2) diffs        
    in sqrt sumSquares
                       
                       
                       


             





{-
-- sample dynamic network specifications : 

-- a. makes "v" a neighbour of "u" iff (pred u v)
predicateNetwork :: (a -> a -> Bool) -> Vector a -> Vector (Vector a)
predicateNetwork pred agents = map (\p -> filter (pred p) agents) agents 

-- b . makes u and v neighbours iff label u = label v
equivalenceClass :: (Eq b) => (a :-> b) -> Vector a -> Vector (Vector a)
equivalenceClass label = predicateNetwork ((==) `on` (get label))
    

-- c. makes person i belong to neighbourhood j of n iff i % n = j
evenlyDistribute people nbhds = map (nbhds!) $ map (`mod` n) (fromList [0 .. nPeople-1]) where
    n = length nbhds
    nPeople = length people

-}