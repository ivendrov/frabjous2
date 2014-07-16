-----------------------------------------------------------------------------
-- |
-- Module      :  Frabjous.StdLib.Internal
-- Copyright   :  (c) University of Saskatchewan 2014
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

module Frabjous.StdLib.Internal -- TODO only export what needs to be exported
where

-- NOTE: by default, sequence operations are VECTOR ones
import Prelude hiding ((.), id, all, foldr)
import Data.Foldable
import Control.Monad.Random (Rand, runRand, RandT, runRandT, getSplit, evalRand)
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Control.Wire
import qualified Data.List as List
import System.IO

import Data.Tuple (swap)
import qualified Data.Traversable as Traversable

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import qualified Frabjous.Compiler.Syntax as Syntax


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
purifyRandom :: (RandomGen g) => WireM (Rand g) a b -> g -> WireP a b
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


type RemovalWire a e= ModelWire (ModelOutput a e) IntSet
type AdditionWire a e= ModelWire (ModelOutput a e) [a]
data PopulationState a e= PopulationState { agentWires :: Collection (AgentWire (ModelOutput a e) a),
                                                 removal :: RemovalWire a e, --death
                                                 addition :: AdditionWire a e} 
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
evolvePopulation :: (ModelOutput a e-> ReactiveOutput a) ->
                     (StdGen -> ID -> a -> AgentWire (ModelOutput a e) a) ->
                     PopulationState a e ->
                        PopulationWire (ModelOutput a e) a 
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

-------------------------------
-- B. Networks
-------------------------------

data Edge e = Edge { index :: (Int, Int), edge :: e}
data Ref a e = Ref { refIndex :: Int, refAgent :: a, refEdge :: e}

reverseEdge :: Edge e -> Edge e
reverseEdge e = e {index = swap (index e)}


-- | gets the agents from an adjacency list of edges 
agents = fmap (refAgent)


-- | The type of adjacency lists in a network. Distinguishes between agents connected to exactly one, possibly one, or many
-- | other agents

data Adj e = 
          One e 
        | MaybeOne (Maybe e) 
        | Many [e] 
        
instance Functor Adj where
        fmap f (One e) = One (f e)
        fmap f (MaybeOne e) = MaybeOne (fmap f e)
        fmap f (Many es) = Many (fmap f es)
        
-- accessors
fromMany :: Adj e -> Maybe [e]
fromMany (Many es) = Just es
fromMany _ = Nothing

fromMaybeOne :: Adj e -> Maybe (Maybe e)
fromMaybeOne (MaybeOne e) = Just e
fromMaybeOne _ = Nothing

fromOne :: Adj e -> Maybe e
fromOne (One e) = Just e
fromOne _ = Nothing

                


-- | network methods
view1, view2, viewSymmetric :: Network e -> Int -> Collection a -> Adj (Ref a e)
addEdges :: [Edge e] ->Network e -> Network e
removeEdges :: [(Int, Int)] -> Network e -> Network e
toIds :: Network e -> [(Int, Int)]
    
    -- TODO removeVertices1, removeVertices2,fromEdges
    
data Network e = Network { accesses :: (Syntax.Link, Syntax.Link), vertices1, vertices2 :: [Int], edges :: [Edge e]}


     
addEdges es network = network {edges = es ++ (edges network)}
removeEdges lst network = network {edges = filter (\e -> not $ (index e) `List.elem` lst) (edges network)}
     
view1 (Network {accesses = (a1, _), edges}) i collection = 
        case a1 of
                Syntax.One -> undefined -- TODO
                Syntax.MaybeOne -> undefined -- TODO
                Syntax.Many -> Many . map (toRef collection) . filter (\e -> fst (index e) == i) $ edges -- TODO optimize by precomputing!
      where toRef collection (Edge (_, i) e) = Ref i (collection IntMap.! i) e
view2 n@(Network {accesses, edges}) = view1 (n {accesses = (swap accesses), edges = (map reverseEdge edges)})

viewSymmetric n = view1 (n { edges = edges n ++ (map reverseEdge (edges n))} )

toIds = map (index) . edges
        
     

     

-- | helper function, computes an agent's adjacency list in a particular network
networkView :: Int -- ^ the index of the agent from whose perspective we're viewing the network
                -> (Network e -> Int -> Collection a -> Adj (Ref a e))  -- ^ the view function (view1, view2, or viewSymmetric)
                -> (model -> Network e) -- ^ the network accessor
                -> (model -> ReactiveOutput a) -- ^ the (target) population accessor
                -> (AgentInput model a) -> (Adj (Ref a e))
networkView id viewer networkAccess populationAccess s =
         viewer (networkAccess . modelState $ s) id (collection . populationAccess . modelState $ s)

-- | extracts the actual adjacency structure from inside an Adj, throws runtime error if the wrong accessor is used
-- | TODO eliminate runtime error
unsafeFromAdj :: (Adj a -> Maybe b) -- ^ the accessor (one of fromMany, fromMaybeOne, fromOne)
            -> String -- ^ the network access name, used in runtime error message
            -> Adj a -- ^ the Adj instance
            -> b
            
unsafeFromAdj accessor name adj = 
        case accessor adj of 
             Just ans -> ans
             Nothing -> error $  "Runtime error: network access " ++ name ++ " used incorrectly"
           

agentPairs :: Network e -> ReactiveOutput a -> ReactiveOutput a -> [(a, a)]
agentPairs network pop1 pop2 = map peeps (toIds network) where
    peeps (id1, id2) = ((collection pop1) IntMap.! id1, collection pop2 IntMap.! id2)
    


-- C) The model as a whole

data ModelState a e = ModelState { populationWires :: Map String (PopulationWire (ModelOutput a e) a),
                               networkWires :: Map String (ModelWire (ModelOutput a e) (Network e))}
data ModelOutput a e = ModelOutput { populations :: Map String (ReactiveOutput a),
                                 networks :: Map String (Network e)}

evolveModel :: ModelState a e -> ModelWire (ModelOutput a e) (ModelOutput a e)
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

data ModelStructure a e = ModelStructure { populationNames, networkNames :: [String],
                                         removalWires :: Map String (RemovalWire a e),
                                         additionWires :: Map String (AdditionWire a e),
                                         newAgentWires :: Map String (StdGen -> ID -> a -> AgentWire (ModelOutput a e) a),
                                         networkEvolutionWires :: Map String (ModelWire (ModelOutput a e) (Network e)),
                                         networkPopulations :: Map String (String, String)}







type NetworkGenerator a e = IntMap a -> IntMap a -> ModelMonad (Network e)
data InitialState a e = InitialState { initialPopulations :: Map String [a],
                                     initialNetworks :: Map String (NetworkGenerator a e)}




initialModelState :: ModelStructure a e -> InitialState a e -> ModelMonad (ModelState a e, ModelOutput a e)
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


createModel :: ModelStructure a e -> Rand StdGen (InitialState a e) -> StdGen -> WireP () (ModelOutput a e)
createModel modelStructure initialState stdgen = 
    let initState = evalRand initialState stdgen
        initPair = (stdgen, 0)
        initialization = initialModelState modelStructure initState
        ((state,output), afterPair) = runModel initialization initPair
        pureModelWire = purifyModel (evolveModel state) afterPair
    in loopWire output pureModelWire

-- STATISTICS
type ObserverWire a e s = WireP (ModelOutput a e) s 
type Statistics a e = Map String (ObserverWire a e String)


-- TODO function that runs a list of wires and returns a list
-- genericize stepWiresP!
stepWiresP wires dt input = 
    let results = map (\wire -> stepWireP wire dt input) wires
    in (map fst results, map snd results)

-- generates a wire out of a map of statistics wires by pretty-printing the statistics and their names
processStatistics :: Statistics a e -> ObserverWire a e String
processStatistics stats = 
    mkPure $ \dt input -> 
        let results = Map.map (\val -> stepWireP val dt input) stats
            (_, rights) = Map.mapEither fst results
            stats' = Map.map snd results
            output = if (not $ Map.null stats')
                     then (unlines . map (\(k,v) -> k ++ " = " ++ v) . Map.toList $ rights) ++ "\n"
                     else ""
        in (Right output, processStatistics stats')

type ObserverProcess a e = (ObserverWire a e String, Handle)



runModelIO :: WireP () (ModelOutput a e) -> [ObserverProcess a e] -> Double -> IO ()
runModelIO modelWire observers timestep = loop modelWire (map fst observers) where
    handles = map snd observers
    loop w wires = do
      let (mx, w') = stepWireP w timestep ()

      case mx of 
        Left _ -> return ()
        Right x -> do let (outputs, wires') = stepWiresP wires timestep x
                          genAction handle (Right str) = hPutStr handle str
                          genAction _ (Left _) = return ()
                          actions = zipWith genAction handles outputs
                      Traversable.sequence actions
                      loop w' wires'