{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

-- NOTE: by default, sequence operations are VECTOR ones
import Prelude hiding (map, zipWith, length, filter, replicate, takeWhile, 
                       (.), id, unzip, all, any, zipWith3, (++), sum)
import Control.Monad hiding (when)
import Control.Arrow
import Control.Wire
import Text.Printf
import qualified Data.List as List
import Data.Vector hiding (modify)
import Data.Ord
import Data.Monoid
import Data.Graph
import Data.Label (fclabels, mkLabels, get, set, modify, (:->))
import Data.Typeable
import Control.Wire.Classes

import Frabjous



-- ---------------------------------
-- 1. Global Declarations
data State = S | I | R deriving (Eq, Show)


-- ---------------------------------
-- 2. Agent (Local) Declarations


data Person = Person { _income :: Double,
                       _state :: State,
                       _neighbours :: Vector Person,
                       _nbhd :: Nbhd,
                       _idxPerson :: Int -- automatically filled in
                     } deriving Typeable

data Nbhd = Nbhd { _avgIncome :: Double,
                   _residents :: Vector Person,
                   _idxNbhd :: Int -- automatically filled in
                 } deriving Typeable

mkLabels [''Person, ''Nbhd]
                       

-- introduces incorrect ordering. Fix with vertical lens composition

instance Agent Person where
    idx = idxPerson
    localChangeWire = helper incomeWire stateWire where 
                      helper incomeWire stateWire = 
                          mkGen $ \dt x -> do
                            (Right incomeNew, incomeWire') <- stepWire incomeWire dt x
                            (Right stateNew, stateWire') <-  stepWire stateWire dt x
                            return (Right $ x {_income = incomeNew,
                                               _state = stateNew},
                                    helper incomeWire' stateWire')

                      stateWire = statechart state transitions
                                  where infectionRatePerPerson = 0.4
                                        calcInfectionRate :: Person -> Double
                                        calcInfectionRate = (*infectionRatePerPerson) . 
                                                            fromIntegral . 
                                                            length . 
                                                            filter (==I) .
                                                            map (get state) .
                                                            get neighbours
                                        transitions state = 
                                            case state of 
                                              S -> pure I . rateWire' . arr (calcInfectionRate) 
                                              I -> pure R . after 3
                                              R -> pure S . after 3

                      incomeWire = liftA2 (*) (arr (get income)) (arr (get income))
                                
                               

instance Agent Nbhd where
    idx = idxNbhd
    localChangeWire = helper avgIncomeWire where
                    helper avgIncomeWire = 
                        mkGen $ \dt x -> do 
                          (Right avgIncomeNew, avgIncomeWire') <- stepWire avgIncomeWire dt x
                          return (Right $ x {_avgIncome = avgIncomeNew}, 
                                  helper avgIncomeWire')

                    avgIncomeWire = arr (averageIncome . get residents) 
                        where averageIncome people = (sum . map (get income) $ people) 
                                                     / fromIntegral (length people)



-- ---------------------------------
-- 3. Population Declarations

fclabels [d|
            data ModelState = ModelState { peopleState :: WireP (Vector Person) (PopulationOutput Person),
                                           nbhdsState :: WireP (Vector Nbhd) (PopulationOutput Nbhd)}
                              -- TODO add global reactive variables

            data ModelOutput = ModelOutput { people :: Vector Person,
                                             nbhds :: Vector Nbhd}
          |]





initialModelState :: ModelOutput -> ModelState
initialModelState initialOutput  = 
    ModelState (evolvePopulation initialPeople) (evolvePopulation initialNbhds) where
        initialPeople = PopulationState wires removal addition where
            wires = replicate (length (get people initialOutput)) localChangeWire
            removal = arr (filter (\p -> get state p == R))
            addition = never
        initialNbhds = PopulationState wires removal addition where
            wires = replicate (length (get nbhds initialOutput)) localChangeWire
            removal = never
            addition = never

-- ---------------------------------
-- 4. Network Declarations

-- Would be really cool to have this: 
-- network people neighbours; neighbours a1 a2 if (get state a1) == (get state a2)


-- What we'll have currently:
-- network people neighbours by (equivalenceClass state)
-- network people nbhd with nbhds residents by evenlyDistribute




--updateNetwork1 :: ModelOutput :


-- ---------------------------------
-- AUTOMATICALLY GENERATED FROM ABOVE 



evolveModel :: ModelState -> WireP ModelOutput ModelOutput
evolveModel mstate = 
    mkGen $ \dt input -> do
      (Right peopleOutput, peopleState) <- stepWire 
                                           (get peopleState mstate)
                                           dt
                                           (get people input)
      (Right nbhdsOutput, nbhdsState) <- stepWire
                                         (get nbhdsState mstate)
                                         dt
                                         (get nbhds input)
      let peopleDead = get removedIndices peopleOutput
          nbhdsDead = get removedIndices nbhdsOutput
          
          peopleNew = processDeath neighbours peopleNew peopleDead . 
                      processDeath nbhd nbhdsNew nbhdsDead $ get agents peopleOutput        
          nbhdsNew = processDeath residents peopleNew peopleDead $ get agents nbhdsOutput

          
          output = updateNetwork1 . updateNetwork2 $ ModelOutput peopleNew nbhdsNew where
                         updateNetwork1 = modify people (computeNetworkSelf
                                      neighbours
                                      (equivalenceClass state))            
                         updateNetwork2 = modify (pairLabel (people, nbhds)) (computeNetwork
                                                                              (nbhd, residents)
                                                                              evenlyDistribute)

          mstate' = ModelState peopleState nbhdsState
      return (Right output, evolveModel mstate')




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
                             (arr (map (get state))
                              &&& 
                              arr (map (map (get idx) . (get neighbours)))
                              &&& 
                              arr (map (get income))
                             )
                             
                    )
                    &&&
                    (arr (get nbhds) >>> 
                         (arr (map (map (get idx) . (get residents))))
                         &&&
                         (arr (map (get avgIncome)))
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
