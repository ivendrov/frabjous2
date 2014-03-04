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
 (function, 
  never,
  rate,
  rSwitch,
  statechart,
  randomNetwork,
  randomSymmetricNetwork,
  poissonRandomSymmetric)
where

import InternalLibrary
import Prelude hiding ((.), id)
import Control.Monad.Random
import Control.Wire hiding (getRandom, MonadRandom)
import Data.Monoid
import Data.Tuple (swap)

-- 1. WIRE COMBINATORS

function :: (Monad m) => (a -> b) -> Wire e m a b
function = arr

never :: (Monad m, Monoid e) => Wire e m a b
never = Control.Wire.empty



-- a wire that takes as input the rate of infection, and produces a unit value with a rate
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
                     computeWire state = transitions state <|> pure state


-- 2. NETWORKS

--  A) Library functions for creation 

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