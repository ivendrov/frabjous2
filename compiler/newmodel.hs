{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}


import Control.Wire hiding (getRandom)
import Control.Monad hiding (when)
import Control.Monad.Random hiding (fromList)
import Control.Monad.Identity (Identity)

-- NOTE: by default, sequence operations are VECTOR ones
import Prelude hiding (map, zipWith, length, filter, replicate, takeWhile, 
                       (.), id, unzip, all, any, zipWith3, (++), sum)
import Control.Arrow
import Text.Printf
import qualified Data.List as List
import Data.Vector hiding (modify)
import Data.Ord
import Data.Monoid
import Data.Graph
import Data.Label (fclabels, mkLabels, get, set, modify, (:->))
import Data.Typeable
import Control.Wire.Classes


import StandardLibrary
import InternalLibrary

-- START GENERATED CODE

data State = S | I | R deriving (Eq, Show)



data Person = Person { _income :: Double, _state :: State, _neighbours :: Vector Person, _nbhd :: Nbhd, _idxPerson :: Int } deriving Typeable
data Nbhd = Nbhd { _avgIncome :: Double, _residents :: Vector Person, _idxNbhd :: Int } deriving Typeable

mkLabels [ ''Person, ''Nbhd ]

getIncome = get income
getState = get state
getNeighbours = get neighbours
getNbhd = get nbhd
getAvgIncome = get avgIncome
getResidents = get residents

stateWire = 
 let {income = function (getIncome);state = function (getState);neighbours = function (getNeighbours);nbhd = function (getNbhd);avgIncome = function (getAvgIncome);residents = function (getResidents)} 
 in statechart state transitions
  where infectionRatePerPerson = 0.4
        calcInfectionRate :: Person -> Double
        calcInfectionRate p = infectionRatePerPerson * 
                             (fromIntegral (length (filter (==I) (map (getState) (getNeighbours p)))))
        transitions state = 
              case state of 
                     S -> pure I . rate . arr (calcInfectionRate) 
                     I -> pure R . after 3
                     R -> pure S . after 3
 
incomeWire = 
 let {income = function (getIncome);state = function (getState);neighbours = function (getNeighbours);nbhd = function (getNbhd);avgIncome = function (getAvgIncome);residents = function (getResidents)} 
 in proc input -> do { __0 <- (income) -< input ; returnA -< ( __0 * 3
) } 
avgIncomeWire = 
 let {income = function (getIncome);state = function (getState);neighbours = function (getNeighbours);nbhd = function (getNbhd);avgIncome = function (getAvgIncome);residents = function (getResidents)} 
 in arr (averageIncome . getResidents) 
    where averageIncome people = (sum . map (getIncome) $ people) 
                                  / fromIntegral (length people)
 

instance Agent Person where 
    idx = idxPerson
    localChangeWire = helper stateWire incomeWire where
           helper stateWire incomeWire =
              mkGen $ \dt x -> do
                (Right stateNew, stateWire') <- stepWire stateWire dt x
                (Right incomeNew, incomeWire') <- stepWire incomeWire dt x
                return (Right $ x {_state = stateNew, _income = incomeNew}, helper stateWire' incomeWire')
instance Agent Nbhd where 
    idx = idxNbhd
    localChangeWire = helper avgIncomeWire where
           helper avgIncomeWire =
              mkGen $ \dt x -> do
                (Right avgIncomeNew, avgIncomeWire') <- stepWire avgIncomeWire dt x
                return (Right $ x {_avgIncome = avgIncomeNew}, helper avgIncomeWire')

fclabels [d|
    data ModelState = ModelState { peopleState :: WireP (Vector Person) (PopulationOutput Person), nbhdsState :: WireP (Vector Nbhd) (PopulationOutput Nbhd) }
    data ModelOutput = ModelOutput { people :: Vector Person, nbhds :: Vector Nbhd }
          |]

initialModelState :: ModelOutput -> ModelState
initialModelState initialOutput =
   ModelState (evolvePopulation (PopulationState (replicate (length (get people initialOutput)) localChangeWire) peopleRemove peopleAdd)) (evolvePopulation (PopulationState (replicate (length (get nbhds initialOutput)) localChangeWire) nbhdsRemove nbhdsAdd)) where{ peopleRemove = arr (filter (\ p -> getState p == R)) ; peopleAdd = never ; nbhdsRemove = never ; nbhdsAdd = never }

evolveModel :: ModelState -> WireP ModelOutput ModelOutput
evolveModel mstate =
  mkGen $ \dt input -> do
    (Right peopleOutput, peopleState) <- stepWire (get peopleState mstate) dt (get people input)
    (Right nbhdsOutput, nbhdsState) <- stepWire (get nbhdsState mstate) dt (get nbhds input)
    let
      peopleNew = processDeath nbhd nbhdsNew (get removedIndices nbhdsOutput) $ get agents peopleOutput
      nbhdsNew = processDeath residents peopleNew (get removedIndices peopleOutput) $ get agents nbhdsOutput
      output = modify (pairLabel (people, nbhds)) (computeNetwork (nbhd, residents) network0) $ ModelOutput peopleNew nbhdsNew where{ network0 = evenlyDistribute }
    return (Right output, evolveModel (ModelState peopleState nbhdsState))


-- END GENERATED CODE

modelWire :: WireP ModelOutput ModelOutput
modelWire = evolveModel (initialModelState startingModel)       

model :: WireP () ModelOutput
model = loopWire startingModel modelWire

-- loopWire init transition = a wire that starts with init, and returns the output of transition on itself 
-- every time it is evaluated (have to write own combinator because this arrow doesn't satisfy ArrowLoop
loopWire :: a -> WireP a a -> WireP () a
loopWire init transition = mkPure $ \ dt _ -> 
                           let (Right next', transition') = stepWireP transition dt init
                           in (Right next', loopWire next' transition')
                         
      


     
-- ----------------------
-- 5. I/O 
wire :: Int ->  WireP () String
wire n = forI n . arr show . 
         (model >>> 
                    (arr (get people) >>> 

                             (arr (map (get state)))
                             &&& 
			     (arr (map (get income)))                          
                             
                             
                    )
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
