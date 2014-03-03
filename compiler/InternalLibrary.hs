-----------------------------------------------------------------------------
-- |
-- Module      :  InternalLibrary
-- Copyright   :  (c) University of Saskatchewan 2013
-- 
-- Maintainer  :  ivan.vendrov@usask.ca
--
-- Library of internal functions (used by compiler-generated Haskell code, but NOT
--  accessible to the Frabjous programmer)
--------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module InternalLibrary -- TODO only export what needs to be exported
where

-- NOTE: by default, sequence operations are VECTOR ones
import Prelude hiding ((.), id, all, foldr)
import Data.Foldable
import Control.Monad hiding (when)
import Control.Monad.Random (RandomGen, Rand, runRand, RandT, runRandT, getSplit)
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Control.Monad.Identity (Identity)
import Control.Arrow
import Control.Wire
import Text.Printf
import qualified Data.List as List

import Data.Ord
import Data.Monoid
import Data.Maybe (isJust, fromJust)
import Data.Function (on)
import Data.Tuple (swap)
import Data.Label
import Data.Typeable
import Data.Either (partitionEithers)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet


-- FRABJOUS STANDARD LIBRARY

------------------------
-- A. Wire Combinators --
------------------------



-- purifyRandom wire gen takes a wire in the Rand monad and an initial generator,
-- and makes it into a pure wire.
purifyRandom :: (RandomGen g) => Wire e (Rand g) a b -> g -> Wire e Identity a b
purifyRandom wire gen = mkPure $ \dt x ->
                                    let ((output, wire'), gen') = runRand (stepWire wire dt x) gen
                                    in (output, purifyRandom wire' gen')


   


                    


-- steps each wire by the given timestep
stepWiresP wires dt inputs = 
    let results = IntMap.mapWithKey (\key val -> stepWireP val dt (inputs IntMap.! key)) wires
        (lefts, rights) = IntMap.mapEither fst results 
        wires' = IntMap.map snd results
        value = if (IntMap.null lefts) then rights
                else error "wires in stepWiresP must produce"  -- if a single wire doesn't produce, nothing produces
       
        in (Right value, wires')







 
-------------------------------
-- B. Agents and Populations
-------------------------------
data AgentInput model a = AgentInput {modelState :: model,
                                      prevState :: a}

type AgentWire model a = WireP (AgentInput model a) a
type ID = Int -- agent ID


type Collection a = IntMap a
                                                
data ReactiveOutput a = ReactiveOutput {collection :: Collection a,
                                        removed, added :: IntSet}
-- TODO add other accessors to reactiveOutput 
type ReactiveCollection input a = WireP input (ReactiveOutput a)






data PopulationState model a = PopulationState { agentWires :: Collection (AgentWire model a),
                                                 removal :: WireP model IntSet, --death
                                                 addition :: WireP model [a]} 
                                                 -- TODO have addition also return birth-added links to specific networks


-- | remove a set of keys from a map
removeSet :: IntMap a -> IntSet -> IntMap a
removeSet map set = foldr IntMap.delete map (IntSet.toList set)



-- | generate a new agent with a new ID and random seed
genNewAgent :: RandomGen g => (g -> ID -> AgentWire input a) -> RandT g (State ID) (ID, AgentWire input a)
genNewAgent f = do 
  i <- State.get
  State.modify (+1)
  g <- getSplit
  return (i, f g i)

genNewAgents n f = replicateM n (genNewAgent f)

type ModelMonad  = RandT StdGen (State ID)

type ModelWire = Wire LastException ModelMonad

runModel :: ModelMonad a -> (StdGen, ID) -> (a,(StdGen, ID))
runModel m (gen, id) = let state_output_gen = runRandT m gen
                           ((output, gen'), id') = (State.runState state_output_gen id)
                       in (output, (gen', id'))
-- purifyModel wire gen takes a wire in the Model monad and an initial generator and state,
-- and makes it into a pure wire.
purifyModel :: ModelWire a b -> (StdGen, ID) -> WireP a b
purifyModel wire pair = mkPure $ \dt x ->
                                    let ((output, wire'), pair') = runModel (stepWire wire dt x) pair
                                    in (output, purifyModel wire' pair')



type PopulationWire model a  = 
    ModelWire model (ReactiveOutput a)

-- evolve the local properties of the population (unrelated to networks)
-- i.e modify each agent locally, then apply death and birth 
evolvePopulation :: (model -> ReactiveOutput a) ->
                     (StdGen -> ID -> AgentWire model a) ->
                     PopulationState model a ->
                        PopulationWire model a 
evolvePopulation extractPop createAgent state = helper state where
    helper  (PopulationState agentWires removal addition) = 
        mkGen $ \dt modelStateP -> do
          let prevAgents = collection . extractPop  $ modelStateP
              (Right agents', agentWires') = stepWiresP agentWires dt (IntMap.map (AgentInput modelStateP) prevAgents)
              (mdeadAgents, removal') = stepWireP removal dt modelStateP
              (mnewAgents, addition') = stepWireP addition dt modelStateP
              deadAgents = case mdeadAgents of Left _ -> IntSet.empty
                                               Right as -> as
              newAgents = case mnewAgents of Left _ -> []
                                             Right as -> as
          newAgentWires <- genNewAgents (length newAgents) createAgent
          let newAgentIndices = map fst newAgentWires
              output = ReactiveOutput {collection = (removeSet agents' deadAgents) `IntMap.union` 
                                                 IntMap.fromList (zip newAgentIndices newAgents),
                                       removed = deadAgents,
                                       added = (IntSet.fromList newAgentIndices)}
              newWires = removeSet agentWires' deadAgents `IntMap.union` IntMap.fromList newAgentWires
                           
          return (Right output, helper (PopulationState newWires removal' addition'))









-- C. Networks
  

type ToMany = IntMap IntSet
networkTranspose :: (Network n) => n -> n
networkTranspose n  = fromEdges (vertices n) . map swap . toEdges $ n
--type ToOne = IntMap Int
--type ToMaybeOne = IntMap (Maybe Int)

-- represents a general interface to a directed network, with (potentially) distinct sets
-- of source and destination vertices
class Network n where 
    vertices :: n -> [Int] 

    view1 :: n -> Int -> Collection a -> [a]
    view2 :: n -> Int -> Collection a -> [a]

    addEdges1 :: ToMany -> n -> n
    addEdges2 :: ToMany -> n -> n

    -- | removes a set of vertices from the first population
    removeVertices1 :: n -> [Int] -> n

    -- | removes a set of vertices from the second population
    removeVertices2 :: n -> [Int] -> n

    fromEdges :: [Int] -> [(Int, Int)] -> n
    toEdges :: n -> [(Int, Int)]

type SymmetricNetwork = ToMany
instance Network SymmetricNetwork where
    vertices = IntMap.keys 

    view1 network index collection = map (collection IntMap.!) $ IntSet.elems (network IntMap.! index)
    view2 = view1
    addEdges1 edges network = let u = IntMap.unionWith IntSet.union 
                              in network `u` edges `u` (networkTranspose edges)
    addEdges2 = addEdges1

    removeVertices1 = foldr IntMap.delete
    removeVertices2 network indices = IntMap.map (IntSet.\\ (IntSet.fromList indices)) network

    toEdges  = List.concatMap (\(i, adj) -> map ((,) i) (IntSet.toList adj)) . IntMap.toList
    fromEdges vertices edges = let adjLists = IntMap.fromList (zip vertices (repeat IntSet.empty))
                                 in foldr (\(i1,i2) -> IntMap.adjust (IntSet.insert i2) i1) 
                                    adjLists 
                                    edges
                                    
--type NetworkWire model = WireP model Network



{- 
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

-}