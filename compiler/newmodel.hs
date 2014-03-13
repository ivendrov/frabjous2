{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Arrows #-}


module Main where

import Control.Wire hiding (getRandom)
import Control.Monad hiding (when, mapM, sequence)
import Control.Monad.Random hiding (fromList)
import Control.Monad.Identity (Identity)

-- NOTE: by default, sequence operations are VECTOR ones
import Prelude hiding ((.), id, mapM, sequence)
import Control.Arrow
import qualified Data.List as List
import Data.Either (rights)
import qualified Data.Traversable as Traversable
import Control.Wire.Classes
import System.IO
import System.Environment (getArgs)


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

personWire g id = purifyRandom (helper stateWire incomeWire) g where
           helper stateWire incomeWire =
              mkGen $ \dt x -> do
                (Right stateNew, stateWire') <- stepWire stateWire dt x
                (Right incomeNew, incomeWire') <- stepWire incomeWire dt x
                return (Right $ (prevState x) {getState = stateNew, getIncome = incomeNew}, helper stateWire' incomeWire')
           income = function (getIncome . prevState)
           state = function (getState . prevState)
           neighbours = networkView id view1 neighboursNetwork people

           stateWire = statechart state transitions
               where infectionRatePerPerson = 0.4
                     calcInfectionRate = proc input -> do
                                           nbs <- neighbours -< input 
                                           returnA -< infectionRatePerPerson * 
                                                       (fromIntegral (length (filter (==I) (map getState nbs))))
                     transitions state = 
                         case state of 
                           S -> pure I . rate . calcInfectionRate
                           I -> pure R . after 3
                           R -> pure S . after 3
 
           incomeWire = proc input -> do { __0 <- income -< input ; returnA -< ( __0 * 3) } 




nbhdWire g id = purifyRandom (helper avgIncomeWire) g where
           helper avgIncomeWire =
              mkGen $ \dt x -> do
                (Right avgIncomeNew, avgIncomeWire') <- stepWire avgIncomeWire dt x
                return (Right $ (prevState x) {getAvgIncome = avgIncomeNew}, helper avgIncomeWire')
           
           avgIncomeWire = pure 0 -- function (averageIncome . getResidents) 
               where averageIncome people = (sum . map (getIncome) $ people) / fromIntegral (length people) 




modelStructure = ModelStructure {
                   populationNames = ["people", "nbhds"],
                   networkNames = ["neighboursNetwork"],
                   removalWires = Map.fromList [("people", peopleRemove), ("nbhds", nbhdsRemove)],
                   additionWires = Map.fromList [("people", peopleAdd), ("nbhds", nbhdsRemove)],
                   newAgentWires = Map.fromList [("people", personWire), ("nbhds", nbhdWire)],
                   networkEvolutionWires = Map.fromList [("neighboursNetwork", neighboursNetworkWire)],
                   networkPopulations = Map.fromList [("neighboursNetwork", ("people", "people"))]
                 } where 
    peopleRemove = never -- arr (IntMap.keysSet . IntMap.filter (\p -> getState p == R) . collection . people) 
    peopleAdd = never --pure [Person {getState = S, getIncome = 0}] . rate . 1
    nbhdsRemove = never 
    nbhdsAdd = never
    neighboursNetworkWire = poissonRandomSymmetric 0.5 people

-- STATISTICS

peopleState = arr (map getState . IntMap.elems . collection) . arr people
percentInfected = arr (\state -> fromIntegral (length (filter (==I) state)) / fromIntegral (length state)) . peopleState

statistics :: Statistics Agent 
statistics = Map.fromList [--("_time", arr show . time),
                           ("peopleState", arr show . peopleState), 
                           ("percentInfected", arr show . percentInfected)]


-- INITIAL STATE

initialState = liftM2 InitialState startingPopulations startingNetworks

startingPopulations = 
    Traversable.sequence $ 
               Map.fromList [("people", startingPeople), ("nbhds", startingNbhds)]

personDistribution = do
  income <- uniform (0, 10)
  state <- frequencies [(I, 0.2), (S, 0.8), (R, 0)]
  return (Person {getIncome = income, 
                  getState =  state})

startingPeople = draw 10 personDistribution

startingNbhds = draw 5 (return (Nbhd 0))

startingNetworks = return $ Map.fromList [("neighboursNetwork", randomSymmetricNetwork 0.2)]


-- OUTPUT
model = createModel modelStructure initialState (mkStdGen 3)

observers = [(processStatistics statistics, stdout)]

main = do 
  args <- getArgs
  if length args < 2 
  then putStrLn "Call with two arguments, duration and timestep"
  else do
      let [t, step] = map read args
      runModelIO (for t . model) observers step