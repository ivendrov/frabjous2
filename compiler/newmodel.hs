{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}


import Control.Wire hiding (getRandom)
import Control.Monad hiding (when)
import Control.Monad.Random hiding (fromList)
import Control.Monad.Identity (Identity)

-- NOTE: by default, sequence operations are VECTOR ones
import Prelude hiding ((.), id)
import Control.Arrow
import Text.Printf
import qualified Data.List as List
import Data.Ord
import Data.Monoid
import Data.Graph
import Data.Label (fclabels, mkLabels, get, set, modify, (:->))
import Data.Typeable
import Control.Wire.Classes


import StandardLibrary
import InternalLibrary


import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

-- START GENERATED CODE

data State = S | I | R deriving (Eq, Show)



data Person = Person { getIncome :: Double, getState :: State} 
data Nbhd = Nbhd { getAvgIncome :: Double} 


data ModelState = ModelState { peopleState :: PopulationWire ModelOutput Person,
                               nbhdsState :: PopulationWire ModelOutput Nbhd }
data ModelOutput = ModelOutput { people :: ReactiveOutput Person, 
                                 nbhds :: ReactiveOutput Nbhd, 
                                 neighboursNetwork :: SymmetricNetwork}

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
 

instance Agent Person ModelOutput where 
    newRandWire id = helper (stateWire id) (incomeWire id) where
           helper stateWire incomeWire =
              mkGen $ \dt x -> do
                (Right stateNew, stateWire') <- stepWire stateWire dt x
                (Right incomeNew, incomeWire') <- stepWire incomeWire dt x
                return (Right $ (prevState x) {getState = stateNew, getIncome = incomeNew}, helper stateWire' incomeWire')
instance Agent Nbhd ModelOutput where 
    newRandWire id = helper (avgIncomeWire id) where
           helper avgIncomeWire =
              mkGen $ \dt x -> do
                (Right avgIncomeNew, avgIncomeWire') <- stepWire avgIncomeWire dt x
                return (Right $ (prevState x) {getAvgIncome = avgIncomeNew}, helper avgIncomeWire')



initialModelState :: [Person] -> [Nbhd] -> ([Int] -> SymmetricNetwork) -> ModelMonad (ModelState, ModelOutput)
initialModelState peopleInit nbhdsInit neighboursNetwork= do 
  let peopleCount = length peopleInit
      nbhdsCount = length nbhdsInit
  peopleIndexed <- genNewAgents peopleCount
  nbhdsIndexed <- genNewAgents nbhdsCount
  let (peopleIDs, initialPeople) =  unzip peopleIndexed
      (nbhdIDs, initialNbhds) = unzip nbhdsIndexed
      peopleState = PopulationState (IntMap.fromList peopleIndexed) peopleRemove peopleAdd
      nbhdState = PopulationState (IntMap.fromList nbhdsIndexed) nbhdsRemove nbhdsAdd
  return $
   (ModelState (evolvePopulation people peopleState)
              (evolvePopulation nbhds nbhdState),
    ModelOutput {people = ReactiveOutput { collection = IntMap.fromList (zip peopleIDs peopleInit),
                                           removed = IntSet.empty,
                                           added = IntSet.empty},
                 nbhds = ReactiveOutput { collection = IntMap.fromList (zip nbhdIDs nbhdsInit),
                                          removed = IntSet.empty,
                                          added = IntSet.empty},
                 neighboursNetwork = neighboursNetwork peopleIDs
                 })
 where{ peopleRemove = never; -- TODO try death 
        peopleAdd = never ; 
        nbhdsRemove = never ; 
        nbhdsAdd = never}

evolveModel :: ModelState -> ModelWire ModelOutput ModelOutput
evolveModel (ModelState peopleState nbhdsState) =
  mkGen $ \dt input -> do
    (Right peopleOutput, peopleState) <- stepWire peopleState dt input
    (Right nbhdsOutput, nbhdsState) <- stepWire nbhdsState dt input
    let output = input { people = peopleOutput, nbhds = nbhdsOutput}  
     -- now update networks to handle birth and death, and THEN run the network evolution wires
     -- output = modify (pairLabel (people, nbhds)) (computeNetwork (nbhd, residents) network0) $ ModelOutput peopleNew nbhdsNew where{ network0 = evenlyDistribute }
    return (Right output, evolveModel (ModelState peopleState nbhdsState))


-- END GENERATED CODE

model :: WireP () ModelOutput
model = let initPair = (mkStdGen 4, 0)
            initialization = initialModelState startingPeople startingNbhds startingNetwork
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
                             
                             
                    )
                    &&&
                    (arr neighboursNetwork) 
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


startingPeople = zipWith Person
                 startingIncomes
                 startingStates
    where size = 5
          numInfected = 1
          startingIncomes = [1 .. size]
          startingStates = replicate numInfected I ++ replicate (size-numInfected) S

startingNbhds = []
startingNetwork = randomNetwork (mkStdGen 1) 0.3

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