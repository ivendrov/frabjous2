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
import Control.Monad.Random (RandomGen, Rand, runRand, RandT, runRandT, getSplit, evalRand)
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Control.Monad.Identity (Identity)
import Control.Arrow
import Control.Wire
import Text.Printf
import qualified Data.List as List
import System.IO

import Data.Ord
import Data.Monoid
import Data.Maybe (isJust, fromJust)
import Data.Function (on)
import Data.Tuple (swap)
import Data.Label
import Data.Typeable
import qualified Data.Traversable as Traversable
import Data.Either (partitionEithers)

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet


-- FRABJOUS STANDARD LIBRARY

------------------------
-- A. Wire Combinators --
------------------------

-- loopWire init transition = a wire that starts with init, and returns the output of transition on itself 
-- every time it is evaluated (have to write own combinator because this arrow doesn't satisfy ArrowLoop
loopWire :: a -> WireP a a -> WireP () a
loopWire init transition =  forI 1 . pure init <|> result init transition where
    result init transition = mkPure $ \ dt _ -> 
             let (Right next', transition') = stepWireP transition dt init
             in (Right next', result next' transition')


-- purifyRandom wire gen takes a wire in the Rand monad and an initial generator,
-- and makes it into a pure wire.
purifyRandom :: (RandomGen g) => WireM (Rand g) a b -> g -> WireM Identity a b
purifyRandom wire gen = mkPure $ \dt x ->
                                    let ((output, wire'), gen') = runRand (stepWire wire dt x) gen
                                    in (output, purifyRandom wire' gen')


   


                    


-- steps each wire by the given timestep
stepWiresMapP wires dt inputs = 
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
indices :: ReactiveOutput a -> [Int]
indices = IntMap.keys . collection

deadIDs :: ReactiveOutput a -> [Int]
deadIDs = IntSet.toList . removed

current :: ReactiveOutput a -> [a]
current = IntMap.elems . collection
-- TODO add other accessors to reactiveOutput 
type ReactiveCollection input a = WireP input (ReactiveOutput a)


type RemovalWire a = ModelWire (ModelOutput a) IntSet
type AdditionWire a = ModelWire (ModelOutput a) [a]
data PopulationState a = PopulationState { agentWires :: Collection (AgentWire (ModelOutput a) a),
                                                 removal :: RemovalWire a, --death
                                                 addition :: AdditionWire a} 
                                                 -- TODO have addition also return birth-added links to specific networks


-- | remove a set of keys from a map
removeSet :: IntMap a -> IntSet -> IntMap a
removeSet map set = foldr IntMap.delete map (IntSet.toList set)



-- | generate a new agent with a new ID and random seed
genNewAgent :: RandomGen g => (g -> ID -> a -> AgentWire input a) -> a -> RandT g (State ID) (ID, AgentWire input a)
genNewAgent f initAgent = do 
  i <- State.get
  State.modify (+1)
  g <- getSplit
  return (i, f g i initAgent)

genNewAgents f initAgents = sequence (map (genNewAgent f) initAgents)

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
evolvePopulation :: (ModelOutput a -> ReactiveOutput a) ->
                     (StdGen -> ID -> a -> AgentWire (ModelOutput a) a) ->
                     PopulationState a ->
                        PopulationWire (ModelOutput a) a 
evolvePopulation extractPop createAgent state = helper state where
    helper  (PopulationState agentWires removal addition) = 
        mkGen $ \dt modelStateP -> do
          let prevAgents = collection . extractPop  $ modelStateP
              (Right agents', agentWires') = stepWiresMapP agentWires dt (IntMap.map (AgentInput modelStateP) prevAgents)
          (mdeadAgents, removal') <- stepWire removal dt modelStateP
          (mnewAgents, addition') <- stepWire addition dt modelStateP
          let deadAgents = case mdeadAgents of Left _ -> IntSet.empty
                                               Right as -> as
              newAgents = case mnewAgents of Left _ -> []
                                             Right as -> as
          newAgentWires <- genNewAgents createAgent newAgents
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
networkTranspose n  = fromEdges (vertices1 n) (vertices2 n) . map swap . toEdges $ n
--type ToOne = IntMap Int
--type ToMaybeOne = IntMap (Maybe Int)

-- represents a general interface to a directed network, with (potentially) distinct sets
-- of source and destination vertices
class Network n where 
    vertices1 :: n -> [Int] 
    vertices2 :: n -> [Int]

    view1 :: n -> Int -> Collection a -> [a]
    view2 :: n -> Int -> Collection a -> [a]

    addEdges1, addEdges2, removeEdges1, removeEdges2 :: ToMany -> n -> n
    

    -- | removes a set of vertices from the first population
    removeVertices1 :: [Int] -> n -> n

    -- | removes a set of vertices from the second population
    removeVertices2 :: [Int] -> n -> n

    fromEdges :: [Int] -> [Int] -> [(Int, Int)] -> n
    toEdges :: n -> [(Int, Int)]

type SymmetricNetwork = ToMany
instance Network SymmetricNetwork where
    vertices1 = IntMap.keys 
    vertices2 = vertices1

    view1 network index collection = map (collection IntMap.!) $ IntSet.elems (network IntMap.! index)
    view2 = view1

    addEdges1 edges network = let u = IntMap.unionWith IntSet.union 
                              in network `u` edges `u` (networkTranspose edges)
    addEdges2 = addEdges1
    removeEdges1 edges network = let u = IntMap.unionWith IntSet.difference
                                 in network `u` edges `u` (networkTranspose edges)
    removeEdges2 = removeEdges1

    removeVertices1 indices network = foldr IntMap.delete network indices
    removeVertices2 indices network = IntMap.map (IntSet.\\ (IntSet.fromList indices)) network

    toEdges  = List.concatMap (\(i, adj) -> map ((,) i) (IntSet.toList adj)) . IntMap.toList
    fromEdges vertices1 vertices2 edges = let adjLists = IntMap.fromList (zip vertices1 (repeat IntSet.empty))
                                 in foldr (\(i1,i2) -> IntMap.adjust (IntSet.insert i2) i1) 
                                    adjLists 
                                    edges

type ManyToMany = (ToMany, ToMany)
instance Network ManyToMany where
  
    vertices1 = vertices1 . fst
    vertices2 = vertices2 . snd

    view1 = view1 . fst
    view2 = view1 . snd

    addEdges1 edges = let  u = IntMap.unionWith IntSet.union 
                      in u edges *** u (networkTranspose edges)
    addEdges2 edges = let u = IntMap.unionWith IntSet.union
                      in u (networkTranspose edges) *** u edges

    removeEdges1 edges = let u = IntMap.unionWith IntSet.difference
                         in u edges *** u (networkTranspose edges)
    removeEdges2 = removeEdges1 -- TODO fix

    removeVertices1 removed (n1, n2) = (removeVertices1 removed n1, removeVertices2 removed n2)
    removeVertices2 removed (n1, n2) = (removeVertices2 removed n1, removeVertices1 removed n2)

    toEdges = toEdges . fst
    fromEdges vertices1 vertices2 = fromEdges vertices1 vertices1 &&& fromEdges vertices2 vertices2 . map swap

networkView id viewer networkAccess populationAccess = 
    arr (\s -> viewer (networkAccess . modelState $ s) id (collection . populationAccess . modelState $ s))

agentPairs network pop1 pop2 = map peeps (toEdges network) where
    peeps (id1, id2) = (collection pop1 IntMap.! id1, collection pop2 IntMap.! id2)
    


-- C) The model as a whole

data ModelState a = ModelState { populationWires :: Map String (PopulationWire (ModelOutput a) a),
                               networkWires :: Map String (ModelWire (ModelOutput a) ManyToMany)}
data ModelOutput a = ModelOutput { populations :: Map String (ReactiveOutput a),
                                 networks :: Map String ManyToMany}

evolveModel :: ModelState a -> ModelWire (ModelOutput a) (ModelOutput a)
evolveModel (ModelState populationWires networkWires) =
  mkGen $ \dt input -> do
    -- 1. evolve the populations
    populationResults <- Traversable.mapM (\p -> stepWire p dt input) populationWires
    let populationWires' = Map.map snd populationResults
        fromRight (Right x) = x
        popOutputs = Map.map (fromRight . fst) populationResults
        outputAfterPop = input { populations = popOutputs }

    -- 2. evolve the networks
    networkResults <- Traversable.mapM (\n -> stepWire n dt outputAfterPop) networkWires
    let networkWires' = Map.map snd networkResults
        networkOutputs = Map.mapWithKey extractOutput networkResults
        extractOutput name (output, _) = case output of 
                                           Right x -> x
                                           Left _ -> (networks input) ! name -- if network wire produces nothing, use previous value
        output = outputAfterPop {networks = networkOutputs}  

    return (Right output, evolveModel (ModelState populationWires' networkWires'))


mapZipWith :: (Ord k) => (a -> b -> c) -> Map k a -> Map k b -> Map k c
mapZipWith f map1 map2 = Map.mapWithKey (\k -> f (map1 ! k)) map2

mapZipWith3 :: Ord k => (a -> b -> c -> d) -> Map k a -> Map k b -> Map k c -> Map k d
mapZipWith3 f map1 map2 map3= Map.mapWithKey (\k -> f (map1 ! k) (map2 ! k)) map3

data ModelStructure a = ModelStructure { populationNames, networkNames :: [String],
                                         removalWires :: Map String (RemovalWire a),
                                         additionWires :: Map String (AdditionWire a),
                                         newAgentWires :: Map String (StdGen -> ID -> a -> AgentWire (ModelOutput a) a),
                                         networkEvolutionWires :: Map String (ModelWire (ModelOutput a) ManyToMany),
                                         networkPopulations :: Map String (String, String)}







type NetworkGenerator a = IntMap a -> IntMap a -> ModelMonad ManyToMany
data InitialState a = InitialState { initialPopulations :: Map String [a],
                                     initialNetworks :: Map String (NetworkGenerator a)}




initialModelState :: ModelStructure a -> InitialState a  -> ModelMonad (ModelState a, ModelOutput a)
initialModelState structure initialState = do 

  indexedPops <- Traversable.sequence (mapZipWith genNewAgents (newAgentWires structure) 
                                                          (initialPopulations initialState))
  let ids =  Map.map (map fst) indexedPops
      wires = Map.map (map snd) indexedPops
      populationStates = mapZipWith3 PopulationState (Map.map IntMap.fromList indexedPops)
                                                 (removalWires structure)
                                                 (additionWires structure)
      popNames = populationNames structure
      popExtractions = Map.fromList $ zip popNames (map (\name ->(Map.! name) . populations) popNames)
      state = ModelState {populationWires = 
                              mapZipWith3 evolvePopulation popExtractions (newAgentWires structure) populationStates,
                          networkWires = networkEvolutionWires structure}
                                        

      initialPopulationOutput ids initAgents = ReactiveOutput { collection = IntMap.fromList (zip ids initAgents),
                                                                removed = IntSet.empty,
                                                                added = IntSet.empty}
      indexedInitialPops = mapZipWith initialPopulationOutput ids (initialPopulations initialState)
      getInitialPop pop = collection $ indexedInitialPops Map.! pop
      tieWithPopulations initNetwork (pop1, pop2) = initNetwork (getInitialPop pop1) (getInitialPop pop2)
      initNetworkActions = mapZipWith tieWithPopulations (initialNetworks initialState) (networkPopulations structure)
  networks <- Traversable.sequence initNetworkActions
  let initialOutput = ModelOutput {populations = indexedInitialPops,
                                   networks = networks}
  return $
   (state, initialOutput)


createModel :: ModelStructure a -> Rand StdGen (InitialState a) -> StdGen -> WireP () (ModelOutput a)
createModel modelStructure initialState stdgen = 
    let initState = evalRand initialState stdgen
        initPair = (stdgen, 0)
        initialization = initialModelState modelStructure initState
        ((state,output), afterPair) = runModel initialization initPair
        pureModelWire = purifyModel (evolveModel state) afterPair
    in loopWire output pureModelWire

-- STATISTICS
type ObserverWire a s = WireP (ModelOutput a) s 
type Statistics a = Map String (ObserverWire a String)


-- TODO function that runs a list of wires and returns a list
-- genericize stepWiresP!
stepWiresP wires dt input = 
    let results = map (\wire -> stepWireP wire dt input) wires
    in (map fst results, map snd results)

-- generates a wire out of a map of statistics wires by pretty-printing the statistics and their names
processStatistics :: Statistics a -> ObserverWire a String
processStatistics stats = 
    mkPure $ \dt input -> 
        let results = Map.map (\val -> stepWireP val dt input) stats
            (_, rights) = Map.mapEither fst results
            stats' = Map.map snd results
            output = if (not $ Map.null stats')
                     then (unlines . map (\(k,v) -> k ++ " = " ++ v) . Map.toList $ rights) ++ "\n"
                     else ""
        in (Right output, processStatistics stats')

type ObserverProcess a = (ObserverWire a String, Handle)



runModelIO :: WireP () (ModelOutput a) -> [ObserverProcess a] -> Double -> IO ()
runModelIO modelWire observers timestep = loop modelWire (map fst observers) where
    handles = map snd observers
    loop w wires = do
      let (mx, w') = stepWireP w timestep ()

      case mx of 
        Left ex -> return ()
        Right x -> do let (outputs, wires') = stepWiresP wires timestep x
                          genAction handle (Right str) = hPutStr handle str
                          genAction handle (Left _) = return ()
                          actions = zipWith genAction handles outputs
                      Traversable.sequence actions
                      loop w' wires'