{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
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
import Data.Vector
import Data.Ord
import Data.Monoid
import Data.Graph
import Data.Label
import Data.Typeable
import Control.Wire.Classes

import Frabjous


-- USER-SPECIFIED DECLARATIONS
data State = S | I | R deriving (Eq, Show)

fclabels [d|
             data Person = Person { income :: Double,
                                    state :: State,
                                    neighbours :: Vector Person,
                                    nbhd :: Nbhd,
                                    idxPerson :: Int -- automatically filled in
                                  } deriving Typeable

             data Nbhd = Nbhd { avgIncome :: Double,
                                residents :: Vector Person,
                                idxNbhd :: Int -- automatically filled in
                              } deriving Typeable

          |]


                 


-- MESSY PARTS - NOT YET FORMALIZED                              

instance Agent Person where
    idx = idxPerson
    localChangeWire = personWire

instance Agent Nbhd where
    idx = idxNbhd
    localChangeWire = saskatoonWire
    
-- STATECHART specified by a state to wire function,
-- and a "status" wire that does the plumbing to ensure wires are reset, etc
infectionRatePerPerson = 0.4
calcInfectionRate :: Person -> Double
calcInfectionRate = (*infectionRatePerPerson) . 
                    fromIntegral . 
                    length . 
                    filter (==I) .
                    map (get state) .
                    get neighbours

stateToWire :: State -> WireP Person State
stateToWire state =
    transitions <|> pure state where
        transitions = 
            case state of 
              S -> pure I . rateWire' . arr (calcInfectionRate) 
              I -> pure R . after 3
              R -> pure S . after 3
           

-- THIS SPECIFIC EXAMPLE

size = 8




modifyWire :: WireP a b -> a :-> b -> WireP a a
modifyWire w lens = mkPure $ \dt input -> 
                    let (output, w') = stepWireP w dt input
                    in case output of
                         Right o -> (Right (set lens o input), modifyWire w' lens)
                         Left e -> (Left e, modifyWire w' lens)
stateWire ::  WireP Person State
stateWire = mkGen $ \dt x -> stepWire (rSwitch stateToWire (get state x)) dt x

incomeWire :: WireP Person Double
incomeWire = liftA2 (*) (arr (get income)) (arr (get income))

personWire :: WireP Person Person
personWire = modifyWire incomeWire income . modifyWire stateWire state -- introduces incorrect ordering. Fix with vertical lens composition


initialPeople = PopulationState wires removal addition where
    wires = replicate size personWire
    removal = arr (filter (\p -> get state p == R))
    addition = never

averageIncome = sum . map (get income)

saskatoonWire = arr (\sask -> set avgIncome (averageIncome . get residents $ sask) sask)
            
initialNbhds = PopulationState (fromList [saskatoonWire,saskatoonWire]) never never




-- AUTOMATICALLY GENERATED FROM ABOVE 
fclabels [d|
            data ModelState = ModelState { peopleState :: WireP (Vector Person) (PopulationOutput Person),
                                           nbhdsState :: WireP (Vector Nbhd) (PopulationOutput Nbhd)}
                              -- TODO add global reactive variables

            data ModelOutput = ModelOutput { people :: Vector Person,
                                             nbhds :: Vector Nbhd}
          |]

evolveModel :: ModelState -> WireP ModelOutput ModelOutput
evolveModel mstate = 
    mkGen $ \dt input -> do
      (Right peopleOutput, peopleState) <- stepWire 
                                           (get peopleState mstate)
                                           dt
                                           (get people input)
      (Right nbhdOutput, nbhdState) <- stepWire
                                       (get nbhdsState mstate)
                                       dt
                                       (get nbhds input)
      let deadPeople = get removedIndices peopleOutput
          deadNbhds = get removedIndices nbhdOutput
          -- process networks in reponse to death
          newPeople = processDeath neighbours newPeople deadPeople . 
                      processDeath nbhd newNbhds deadNbhds $ people where
                         people = get agents peopleOutput
                                        
       

          newNbhds = processDeath residents newPeople deadPeople $ nbhds where
                         nbhds = get agents nbhdOutput

          -- process network change (just functions for now, perhaps generalize to wires)

          -- example: generate network where edges are between agents with the same state
          newPeople2 = computeNetworkSelf                      
                       neighbours
                       (equivalenceClass state)
                       newPeople
          (newPeople3, newNbhds2) = computeNetwork
                                    (nbhd, residents) 
                                     evenlyDistribute 
                                    (newPeople2, newNbhds)
          -- example: only have saskatoon contain susceptible agents - but where do the rest go? SYMMETRY
 
          
          output = ModelOutput newPeople3 newNbhds2
          mstate' = ModelState peopleState nbhdState
      return (Right output, evolveModel mstate')


initialModel = ModelState (evolvePopulation initialPeople) (evolvePopulation initialNbhds)
                                                                    
                     
    

sirWire :: WireP ModelOutput ModelOutput
sirWire = evolveModel initialModel where              
                            
-- STARTING STATE

fraction = 0.8
sirNetwork =  randomNetwork (mkStdGen 32498394823) fraction size
startingStates = replicate size S // [(0, I)]
startingIncomes = generate size fromIntegral
startingNeighbours = tieMany startingPeople sirNetwork
startingPeople = zipWith5 Person 
                 startingIncomes 
                 startingStates 
                 startingNeighbours 
                 (replicate size saskatoon)
                 (fromList [0 .. size-1])

saskatoon = Nbhd
            (averageIncome startingPeople)
            startingPeople
            0

startingModel = ModelOutput startingPeople (fromList [saskatoon, set idx 1 saskatoon])


sir :: WireP () ModelOutput
sir = loopWire startingModel sirWire

-- loopWire init transition = a wire that starts with init, and returns the output of transition on itself 
-- every time it is evaluated (have to write own combinator because this arrow doesn't satisfy ArrowLoop
loopWire :: a -> WireP a a -> WireP () a
loopWire init transition = mkPure $ \ dt _ -> 
                           let (Right next', transition') = stepWireP transition dt init
                           in (Right next', loopWire next' transition')
                         
      
     
        
test :: WireP () Bool
test = proc _ -> do
         rec a <- delay True . arr (not) . arr (==False)  -< a
         returnA -< a

wire :: Int ->  WireP () String
wire n = forI n . arr show . (((arr (map (get state)) &&& arr (map (map (get idx) . (get neighbours))))  
         . arr (get people))  &&& arr (map (map (get idx) . (get residents)) . (get nbhds))) . sir

control whenInhibited whenProduced wire = loop wire (counterSession 0.2) where
    loop w' session' = do
      (mx, w, session) <- stepSessionP w' session' ()
      case mx of 
        Left ex -> whenInhibited ex
        Right x -> do whenProduced x
                      loop w session
main = do 
  n <- readLn
  putStrLn . show $ sirNetwork 
  control return (putStrLn) $ (wire n)