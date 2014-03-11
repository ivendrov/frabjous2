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
import Data.Either (rights)
import Data.Label (fclabels, mkLabels, get, set, modify, (:->))
import Data.Typeable
import qualified Data.Traversable as Traversable
import Control.Wire.Classes
import System.IO


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



people = (Map.! "people") . populations
nbhds = (Map.! "nbhds") . populations
neighboursNetwork = (Map.! "neighboursNetwork") . networks

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

peopleRemove = never -- arr (IntMap.keysSet . IntMap.filter (\p -> getState p == R) . collection . people) 
peopleAdd = never --pure [Person {getState = S, getIncome = 0}] . rate . 1
nbhdsRemove = never 
nbhdsAdd = never

neighboursNetworkWire = poissonRandomSymmetric 0.5 people


modelStructure = ModelStructure {
                   populationNames = ["people", "nbhds"],
                   networkNames = ["neighboursNetwork"],
                   removalWires = Map.fromList [("people", peopleRemove), ("nbhds", nbhdsRemove)],
                   additionWires = Map.fromList [("people", peopleAdd), ("nbhds", nbhdsRemove)],
                   newAgentWires = Map.fromList [("people", personWire), ("nbhds", nbhdWire)],
                   networkEvolutionWires = Map.fromList [("neighboursNetwork", neighboursNetworkWire)],
                   networkPopulations = Map.fromList [("neighboursNetwork", ("people", "people"))]
                 }

initialState = InitialState startingPopulations startingNetworks



-- END GENERATED CODE


model = createModel modelStructure initialState (mkStdGen 3)
   





peopleState = arr (map getState . IntMap.elems . collection) . arr people
percentInfected = arr (\state -> fromIntegral (length (filter (==I) state)) / fromIntegral (length state)) . peopleState

statistics :: Statistics Agent 
statistics = Map.fromList [("_time", arr show . time),
                           ("peopleState", arr show . peopleState), 
                           ("percentInfected", arr show . percentInfected)]

observers = [(processStatistics statistics, stdout)]

main = do 
  t <- readLn
  runModelIO (for t . model) observers 0.2


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

startingNbhds = map Nbhd (replicate size 0)
    where size = 5

startingNetworks = Map.fromList [("neighboursNetwork", randomSymmetricNetwork 0.4)]