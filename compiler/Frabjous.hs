{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Frabjous -- TODO only export what needs to be exported
where

-- NOTE: by default, sequence operations are VECTOR ones
import Prelude hiding (map, zipWith, length, filter, replicate, takeWhile, 
                       (.), id, unzip, all, any, zipWith3, (++), sum)
import Control.Monad hiding (when)
import Control.Monad.Random hiding (fromList)
import Control.Monad.Identity (Identity)
import Control.Arrow
import Control.Wire hiding (getRandom)
import Text.Printf
import qualified Data.List as List
import Data.Vector
import Data.Ord
import Data.Monoid
import Data.Maybe (isJust, fromJust)
import Data.Function (on)
import Data.Graph
import Data.Tuple (swap)
import Data.Label
import Data.Typeable


-- FRABJOUS STANDARD LIBRARY

----------------
-- A. Utility --
----------------
type AdjList = Vector (Vector Int)
function :: (RandomGen g) => (a -> b) -> Wire e (Rand g) a b -- TODO change to randomness / logging monad
function = arr

--  0) Label generator (used for networks)
pairLabel (l1,l2) = point $ 
               (,) 
               <$> fstL >- l1
               <*> sndL >- l2 where
                   (fstL, sndL) = $(getLabel ''(,))


--  1) Removal

-- removeIdxMap deletions i is the new index of the element 
-- that used to be at the ith position before the elements at indices "deletions" were removed
-- deletions must be in increasing order
removeIdxMap :: Vector Int -> (Int -> Maybe Int)
removeIdxMap deletions i = if (any (==i) deletions) then Nothing
                           else Just $ i - length (takeWhile (<i) deletions)

removeMap size deletions = findIndices (isJust) $ 
                           map (removeIdxMap deletions) (fromList [0 .. size - 1])

remove deletions v = backpermute v (removeMap (length v) deletions)
regenIndices v = map (fromJust) . filter (isJust) . map (removeIdxMap v)



--  2) Wire Combinators
never :: (Monad m, Monoid e) => Wire e m a b
never = Control.Wire.empty



-- a wire that takes as input the rate of infection, and produces a unit value with a rate
-- corresponding to the given time, minus inaccuracy with multiple occurrences in a 
-- single time interval
rate :: (RandomGen g) => Wire LastException (Rand g) Double ()
rate = mkGen $
           \dt lambda -> do 
             e <- getRandom
             return (if (e < 1 - exp (-dt * lambda)) then Right () else Left mempty, rate)


-- purifyRandom wire gen takes a wire in the Rand monad and an initial generator,
-- and makes it into a pure wire.
purifyRandom :: (RandomGen g) => Wire e (Rand g) a b -> g -> Wire e Identity a b
purifyRandom wire gen = mkPure $ \dt x ->
                                    let ((output, wire'), gen') = runRand (stepWire wire dt x) gen
                                    in (output, purifyRandom wire' gen')

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
   


                    

-- Parallel wire operator (just pure wires for now)
par :: Vector (WireP a b) -> WireP (Vector a) (Vector b)
par wires = 
    mkPure $ \dt inputs -> 
        let (value, wires') = stepWiresP wires dt inputs
        in (Right value, par wires')

-- steps each wire by the given timestep
stepWiresP wires dt inputs = 
    let dts = replicate (length wires) dt 
        (values, wires') = unzip $ zipWith3 stepWireP wires dts inputs 
        right x = case x of 
                    Right y -> y
                    Left _ -> error "cannot extract right from a Left contructor"
        isRight x = case x of
                      Right _ -> True
                      Left (Last _) -> False
        value = if (all isRight values) 
                then map right values
                else error "wires in stepWiresP must produce"  -- if a single wire doesn't produce, nothing produces
        in (value, wires')
 

-- B. Agents and Populations

class Agent a where
    idx :: a :-> Int
    localChangeWire :: WireP a a

fclabels [d|
             data PopulationState a = PopulationState { agentWires :: Vector (WireP a a),
                                                        removal :: WireP (Vector a) (Vector a), --death
                                                        addition :: WireP (Vector a) (Vector a)} --birth

             data PopulationOutput a = PopulationOutput { agents :: Vector a,
                                                          removedIndices :: Vector Int}


          |]

-- evolve the local properties of the population (unrelated to networks)
-- i.e modify each agent locally, then apply death and birth (UNIMPLEMENTED)
evolvePopulation :: (Agent a, Monad m) => PopulationState a -> Wire e m (Vector a) (PopulationOutput a)
evolvePopulation (PopulationState agentWires removal addition) = 
    mkPure $ \dt agents ->
        let (agents', agentWires') = stepWiresP agentWires dt agents
            (deadAgents, removal') = stepWireP removal dt agents'
            (newAgents, addition') = stepWireP addition dt agents'
        in
           let removedIndices = case deadAgents of 
                                  Left _ -> fromList []
                                  Right deads -> map (get idx) deads
               agentsAfterRemoval = remove removedIndices agents'
               newborns = case newAgents of
                            Left _ -> fromList []
                            Right as -> as
              
               outputAgents = zipWith (set idx) (fromList [0 .. length agentsAfterRemoval - 1]) agentsAfterRemoval
               newWires = (remove removedIndices agentWires')
               output = PopulationOutput outputAgents removedIndices
                           
           in (Right output, evolvePopulation (PopulationState newWires removal' addition'))

-- C. Networks

--  1) Library functions for creation 
randomNetwork :: RandomGen r => r -> Double -> Int -> AdjList
-- randomNetwork rng fraction size = a random network with the given number of nodes. 
--   each pair of nodes has a "fraction" probability of an edge

randomNetwork rng fraction size =  fromList . List.map (fromList) $ adjList where
    edges' = List.map snd . List.filter ((<fraction) . fst) . List.zip (randoms rng) $ 
            [(u,v) | u <- [0 .. size - 1], 
                     v <- [u+1 .. size -1]]
    edges = edges' List.++ (List.map swap edges')
    adjList = List.map (\i -> List.map (snd) . List.filter (\edge -> fst edge == i) $ edges) [0 .. size -1] 


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



-- 2) Modification

tieMany agents adj = map (map (agents !)) adj
tieOne agents adj = map (agents !) adj
getIdxMany :: (Agent a) => Vector (Vector a) -> Vector (Vector Int)
getIdxMany = map (map (get idx)) 
getIdxOne :: (Agent a) => Vector a -> Vector Int
getIdxOne = map (get idx)
dieMany deads =  map (regenIndices deads)
dieOne deads = map (fromJust . removeIdxMap deads) -- throws exception if parent dies without all children dying




-- computeNetworkSelf computeAdj label as 
-- recomputes the network between agents in as specified by (map (get labelA) as)
-- using computeAdj, and returns the resulting calculation
computeNetworkSelf :: (Agent a, Typeable a, Typeable setA) => 
                      (a :-> setA) 
                      -> (Vector a -> Vector setA) 
                      -> (Vector a -> Vector a)
computeNetworkSelf label computeAdj as = 
    case (gcast label, gcast computeAdj) of
      (Just (label :: a :-> Vector a),
       Just (computeAdj :: Vector a -> Vector (Vector a))) ->
                                         -- many to many case
                                         newAs where
                                             newAs = zipWith (set label) newNeighbours as  
                                             newNeighbours = tieMany newAs newAdj
                                             newAdj = getIdxMany $ computeAdj as
      _ -> error "case not implemented"
 






-- computeNetwork (labelA, labelB) (a :-> setA) (as, bs)
-- recomputes the bipartite network between agents in as and bs specified by 
-- (map (get labelA) as, map (get labelB) bs), 
-- using the given computeAdj to get the first and taking its transpose to get the second 
-- and returns the resulting populations
computeNetwork :: (Agent a, Agent b, Typeable a, Typeable b, Typeable setB, Typeable setA) => 
                         (a :-> setB, b :-> setA) 
                         -> (Vector a -> Vector b -> Vector setB)  
                         -> (Vector a, Vector b)  
                         -> (Vector a, Vector b)
computeNetwork (labelA, labelB) computeAdj (as, bs) = 
    case (gcast labelA, gcast labelB, gcast computeAdj) of
      (Just (labelA :: a :-> b) , 
       Just (labelB :: b :-> Vector a),
       Just (computeAdj :: Vector a -> Vector b -> Vector b)) -> 
           -- many to one version
          (newAs, newBs) where 
              newAs =  zipWith (set labelA) newANeighbours as 
              newANeighbours = tieOne newBs newAAdj
              newBs = zipWith (set labelB) newBNeighbours bs 
              newBNeighbours = tieMany newAs newBAdj
              newAAdj = getIdxOne $ computeAdj as bs
              newBAdj = networkTransposeOneMany newAAdj where
                   networkTransposeOneMany :: Vector Int -> Vector (Vector Int)
                   networkTransposeOneMany vi = map residents (fromList [0.. length vi - 1]) where
                                                  residents i = findIndices (==i) vi -- TODO optimize
      _ -> error "case not implemented yet"
                         
       
processDeath :: (Agent a, Agent b, Typeable setB, Typeable b) =>
                    (a :-> setB) -> Vector b -> Vector Int -> Vector a -> Vector a 
processDeath label newBs deadBs as = 
    case (gcast label) of 
      Just label -> -- case label :: a -> Vector b
          zipWith (set label) newNeighbours as where
                     newNeighbours = tieMany newBs newAdj
                     newAdj = dieMany deadBs oldAdj
                     oldAdj = getIdxMany oldNeighbours
                     oldNeighbours = map (get label) as 
      Nothing -> case (gcast label) of
                   Just label -> -- case label :: a -> b
                        zipWith (set label) newNeighbours as where
                                    newNeighbours = tieOne newBs newAdj
                                    newAdj = dieOne deadBs oldAdj
                                    oldAdj = getIdxOne oldNeighbours
                                    oldNeighbours = map (get label) as
                   Nothing -> error "case not implemented yet"