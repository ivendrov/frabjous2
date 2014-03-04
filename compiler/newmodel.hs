{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}


import Control.Wire hiding (getRandom)
import Control.Monad hiding (when, mapM, sequence)
import Control.Monad.Random hiding (fromList)
import Control.Monad.Identity (Identity)

-- NOTE: by default, sequence operations are VECTOR ones
import Prelude hiding ((.), id, mapM, sequence)
import Control.Arrow
import Text.Printf
import qualified Data.List as List
import Data.Ord
import Data.Monoid
import Data.Graph
import Data.Label (fclabels, mkLabels, get, set, modify, (:->))
import Data.Typeable
import qualified Data.Traversable as Traversable
import Control.Wire.Classes


import StandardLibrary
import InternalLibrary


import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

-- START GENERATED CODE

data State = S | I | R deriving (Eq, Show)



data Agent = Person { getIncome :: Double, getState :: State} 
            | Nbhd { getAvgIncome :: Double} 

data ModelState = ModelState { populationWires :: Map String (PopulationWire ModelOutput Agent),
                               networkWires :: Map String (ModelWire ModelOutput ManyToMany)}
data ModelOutput = ModelOutput { populations :: Map String (ReactiveOutput Agent),
                                 networks :: Map String ManyToMany}

people = (Map.! "people") . populations
nbhds = (Map.! "nbhds") . populations
neighboursNetwork = (Map.! "neighboursNetwork") . networks

popExtractions = Map.fromList[("people", people), ("nbhds", nbhds)]

income = function (getIncome . prevState)
state = function (getState . prevState)
neighbours i = function (\s -> view1 (neighboursNetwork . modelState $ s) i (collection .people . modelState $ s))

stateWire i = statechart state transitions
  where infectionRatePerPerson = 0.4
        calcInfectionRate = proc input -> do
                              nbs <- neighbours i-< input 
                              returnA -< infectionRatePerPerson * 
                                          (fromIntegral (length (filter (==I) (map getState nbs))))
        transitions state = 
              case state of 
                     S -> pure I . rate . calcInfectionRate
                     I -> pure R . after 3
                     R -> pure S . after 3
 
incomeWire i =  proc input -> do { __0 <- income -< input ; returnA -< ( __0 * 3) } 
avgIncomeWire i = pure 0 -- function (averageIncome . getResidents) 
    where averageIncome people = (sum . map (getIncome) $ people) 
                                  / fromIntegral (length people) 
 


personWire g id = purifyRandom (helper (stateWire id) (incomeWire id)) g where
           helper stateWire incomeWire =
              mkGen $ \dt x -> do
                (Right stateNew, stateWire') <- stepWire stateWire dt x
                (Right incomeNew, incomeWire') <- stepWire incomeWire dt x
                return (Right $ (prevState x) {getState = stateNew, getIncome = incomeNew}, helper stateWire' incomeWire')
nbhdWire g id = purifyRandom (helper (avgIncomeWire id)) g where
           helper avgIncomeWire =
              mkGen $ \dt x -> do
                (Right avgIncomeNew, avgIncomeWire') <- stepWire avgIncomeWire dt x
                return (Right $ (prevState x) {getAvgIncome = avgIncomeNew}, helper avgIncomeWire')

peopleRemove = arr (IntMap.keysSet . IntMap.filter (\p -> getState p == R) . collection . people) 
peopleAdd = pure [Person {getState = S, getIncome = 0}] . rate . 1
nbhdsRemove = never 
nbhdsAdd = never


mapZipWith :: (Ord k) => (a -> b -> c) -> Map k a -> Map k b -> Map k c
mapZipWith f map1 map2 = Map.mapWithKey (\k -> f (map1 ! k)) map2

mapZipWith3 :: Ord k => (a -> b -> c -> d) -> Map k a -> Map k b -> Map k c -> Map k d
mapZipWith3 f map1 map2 map3= Map.mapWithKey (\k -> f (map1 ! k) (map2 ! k)) map3

removalWires = Map.fromList [("people", peopleRemove), ("nbhds", nbhdsRemove)]
additionWires = Map.fromList [("people", peopleAdd), ("nbhds", nbhdsRemove)]

newAgentWires :: Map String (StdGen -> ID -> AgentWire ModelOutput Agent)
newAgentWires = Map.fromList [("people", personWire), ("nbhds", nbhdWire)]


neighboursNetworkWire = poissonRandomSymmetric people 0.5
networkWiresMap = Map.fromList [("neighboursNetwork", neighboursNetworkWire)]


initialModelState :: Map String [Agent] -> ([Int] -> [Int] -> ModelMonad ManyToMany) -> ModelMonad (ModelState, ModelOutput)
initialModelState initialPops network = do 
  let initialCounts = Map.map length initialPops 
  indexedPops <- Traversable.sequence (mapZipWith genNewAgents initialCounts newAgentWires)
  let ids =  Map.map (map fst) indexedPops
      wires = Map.map (map snd) indexedPops
      populationStates = mapZipWith3 PopulationState (Map.map IntMap.fromList indexedPops)
                                                 removalWires
                                                 additionWires
      state = ModelState {populationWires = 
                              mapZipWith3 evolvePopulation popExtractions newAgentWires populationStates,
                          networkWires = networkWiresMap}
                                        

      initialPopulationOutput ids initAgents = ReactiveOutput { collection = IntMap.fromList (zip ids initAgents),
                                                                removed = IntSet.empty,
                                                                added = IntSet.empty}
      indexedInitialPops = mapZipWith initialPopulationOutput ids initialPops
  network <- network (ids Map.! "people") (ids Map.! "people")
  let initialOutput = ModelOutput {populations = indexedInitialPops,
                                   networks = Map.fromList 
                                              [("neighboursNetwork", network)]}
  return $
   (state, initialOutput)

evolveModel :: ModelState -> ModelWire ModelOutput ModelOutput
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
        networkOutputs = Map.map (fromRight . fst) networkResults
        output = outputAfterPop {networks = networkOutputs}  

    return (Right output, evolveModel (ModelState populationWires' networkWires'))


-- END GENERATED CODE

model :: WireP () ModelOutput
model = let initPair = (mkStdGen 4, 0)
            initialization = initialModelState startingPopulations startingNetwork
            ((state,output), afterPair) = runModel initialization initPair
            pureModelWire = purifyModel (evolveModel state) afterPair
         in loopWire output pureModelWire

-- loopWire init transition = a wire that starts with init, and returns the output of transition on itself 
-- every time it is evaluated (have to write own combinator because this arrow doesn't satisfy ArrowLoop
loopWire :: a -> WireP a a -> WireP () a
loopWire init transition =  forI 1 . pure init <|> result init transition where
    result init transition = mkPure $ \ dt _ -> 
             let (Right next', transition') = stepWireP transition dt init
             in (Right next', result next' transition')
                         
      




    
-- ----------------------
-- 5. I/O 
wire :: Int ->  WireP () String
wire n = forI n . arr show . 
         (model >>> 
                    (arr people >>> 

                             (arr (map (getState) . IntMap.elems . collection))
                             {-&&& 
			     (arr (map (getIncome) . IntMap.elems . collection))       -}                   
                             
                             
                    ){-
                    &&&
                    (arr ( fst . neighboursNetwork)) -}
                   )
  

control whenInhibited whenProduced wire = loop wire (counterSession 0.2) where
    loop w' session' = do
      (mx, w, session) <- stepSessionP w' session' ()
      case mx of 
        Left ex -> whenInhibited ex
        Right x -> do whenProduced x
                      loop w session
main = do 
  n <- readLn
  control return (putStrLn) $ (wire n)


-- ----------------------
-- 6. Run Configuration

-- STARTING STATE
startingPopulations = Map.fromList [("people", startingPeople), ("nbhds", startingNbhds)]

startingPeople = zipWith Person
                 startingIncomes
                 startingStates
    where size = 5
          numInfected = 1
          startingIncomes = [1 .. size]
          startingStates = replicate numInfected I ++ replicate (size-numInfected) S

startingNbhds = []
startingNetwork = randomSymmetricNetwork 0.4

{-
startingPeople = zipWith5 Person 
                 startingIncomes 
                 startingStates 
                 startingNeighbours 
                 (replicate size (startingNbhds ! 0))
                 (fromList [0 .. size-1]) 
    where size = 8
          fraction = 0.8
          sirNetwork =  randomNetwork (mkStdGen 32498394823) fraction size
          startingStates = replicate size S // [(0, I)]
          startingIncomes = generate size fromIntegral
          startingNeighbours = tieMany startingPeople sirNetwork

startingNbhds = zipWith3 Nbhd
                (replicate initNumNbhds 0)
                (replicate initNumNbhds (fromList []))
                (fromList [0..initNumNbhds-1]) 
    where initNumNbhds = 2

startingModel = ModelOutput startingPeople startingNbhds
-}