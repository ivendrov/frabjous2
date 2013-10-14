{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Main where

import Control.Monad hiding (when)
import Control.Monad.Identity (Identity)
import Control.Arrow
import Control.Wire
import System.Random (mkStdGen)
import Text.Printf
import Prelude hiding ( (.), id)
import Data.List as List
import Data.Vector as Vector
import Data.Ord
import Data.Monoid
import Data.Maybe (isJust, fromJust)
import Data.Function (on)
import Data.Graph
import Data.Tuple (swap)

import Control.Wire.Classes

-- GENERAL NETWORK STUFF (should work both for 2D (DPH) and 1D (GPU) network representations)
type Network = Vector (Vector Int)


genNeighbours :: Vector a -> Network -> Vector (Vector a)
genNeighbours as adj = Vector.map (Vector.map (as !)) adj

neighboursAdj :: Vector Person -> Network
neighboursAdj = Vector.map (Vector.map idx . neighbours) 



-- SIR combinators / data 
data Person = Person { income :: Double,
                     state :: State,
                     neighbours :: Vector Person,
                     idxPerson :: Int} deriving (Show)

data PersonBase = PersonBase {income' :: Double,
                              state' :: State}

data PersonNetwork = PersonNetwork {neighours' :: Vector Person,
                                    idxPerson' :: Int}

genPerson (PersonBase inc st) (PersonNetwork nbs id) = Person inc st nbs id
                                               

class Agent a where
    idx :: a -> Int

instance Agent Person where
    idx = idxPerson
               


      
data State = S | I | R deriving (Eq, Show)


-- rate generates events according to a poisson process with parameter 1/mu
-- problem : what about multiple occurrences in a single time interval?? 
rate :: (Monoid e, RandomGen g) => Time -> g -> Event e m a
rate mu g
     | mu <= 0 =  error "mean time cannot be negative"
     | otherwise = mkPure $
       \dt x -> 
        let (e,g') = random g in
        (if (e < 1 - exp (-dt / mu)) then Right x else Left mempty, rate mu g')
  
rate' mu = rate mu (mkStdGen 3)

-- a wire that takes as input the rate of infection, and produces a unit value with a rate
-- corresponding to the given time, minus inaccuracy with multiple occurrences in a 
-- single time interval
rateWire ::  (RandomGen g) => g -> WireP Double ()
rateWire g = mkPure $
           \dt lambda -> 
               let (e, g') = random g in
               (if (e < 1 - exp (-dt * lambda)) then Right () else Left mempty, rateWire g')

rateWire' = rateWire (mkStdGen 3)
               




-- STATECHART specified by a state to wire function,
-- and a "status" wire that does the plumbing to ensure wires are reset, etc
infectionRatePerPerson = 0.4
calcInfectionRate :: Person -> Double
calcInfectionRate = (*infectionRatePerPerson) . 
                    fromIntegral . 
                    Vector.length . 
                    Vector.filter (==I) .
                    Vector.map (state) .
                    neighbours

--never = Control.Wire.empty -- event that never triggers 

stateToWire :: State -> WireP Person State
stateToWire state =
    transitions <|> pure state where
        transitions = 
            case state of 
              S -> pure I . rateWire' . arr (calcInfectionRate) 
              I -> pure R . after 3
              R -> pure S . after 3




-- rSwitch switches between wires based on the given discriminator function "computeWire"
-- currently, every time its current wire changes output, it plugs the output into computeWire
-- to determine the wire to switch to.
-- TODO: decompose the "every time wire changes" and the "select wire based on a function" 
--       into two separate combinators. The second combinator could be used to implement
--       phase shifts in other variables (e.g. income calculation as child vs adult)
rSwitch computeWire initialState = helper' initialState (stateToWire initialState) where
    helper' state currentWire = 
        mkGen $ \dt x -> do
          (output, currentWire') <- stepWire currentWire dt x
          case output of 
            Left _ -> error "status wire should never inhibit"
            Right newState -> 
                return (output, 
                        helper' 
                          newState           
                          (if (newState == state) then currentWire' else stateToWire newState))

-- Parallel wire operator (just pure wires for now)
par :: Vector (WireP a b) -> WireP (Vector a) (Vector b)
par wires = 
    mkPure $ \dt inputs -> 
        let (value, wires') = stepWiresP wires dt inputs
        in (Right value, par wires')

-- steps each wire by the given timestep
stepWiresP wires dt inputs = 
    let dts = Vector.replicate (Vector.length wires) dt 
        (values, wires') = Vector.unzip $ Vector.zipWith3 stepWireP wires dts inputs 
        right x = case x of 
                    Right y -> y
                    Left _ -> error "cannot extract right from a Left contructor"
        isRight x = case x of
                      Right _ -> True
                      Left (Last _) -> False
        value = if (Vector.all isRight values) 
                then Vector.map right values
                else error "wires in stepWiresP must produce"  -- if a single wire doesn't produce, nothing produces
        in (value, wires')
 
           

-- THIS SPECIFIC EXAMPLE
randomNetwork :: RandomGen r => r -> Double -> Int -> Network
-- randomNetwork rng fraction size = a random network with the given number of nodes. 
--   each pair of nodes has a "fraction" probability of an edge

randomNetwork rng fraction size =  fromList . List.map (fromList) $ adjList where
    edges' = List.map snd . List.filter ((<fraction) . fst) . List.zip (randoms rng) $ 
            [(u,v) | u <- [0 .. size - 1], 
                     v <- [u+1 .. size -1]]
    edges = edges' List.++ (List.map swap edges')
    adjList = List.map (\i -> List.map (snd) . List.filter (\edge -> fst edge == i) $ edges) [0 .. size -1] 



size = 4
fraction = 0.8
sirNetwork =  randomNetwork (mkStdGen 32498394823) fraction size
startingStates = Vector.replicate size S // [(0, I)]
startingIncomes = Vector.generate size fromIntegral
startingNeighbours = genNeighbours startingAgents sirNetwork
startingAgents = Vector.zipWith4 Person 
                 startingIncomes 
                 startingStates 
                 startingNeighbours 
                 (fromList [0 .. size-1])


stateWire ::  WireP Person State
stateWire = mkGen $ \dt x -> stepWire (rSwitch stateToWire (state x)) dt x

incomeWire :: WireP Person Double
incomeWire = liftA2 (*) (arr income) (arr income)

personWire :: WireP Person PersonBase
personWire = liftA2 (PersonBase) incomeWire stateWire

personsWire :: WireP (Vector Person) (Vector PersonBase)
personsWire = par (Vector.replicate size personWire)


-- DEATH & BIRTH - general functions
-- removeIdxMap deletions i is the new index of the element 
-- that used to be at the ith position before the elements at indices "deletions" were removed
-- deletions must be in increasing order
removeIdxMap :: Vector Int -> (Int -> Maybe Int)
removeIdxMap deletions i = if (Vector.any (==i) deletions) then Nothing
                           else Just $ i - Vector.length (Vector.takeWhile (<i) deletions)

removeMap size deletions = Vector.findIndices (isJust) $ 
                           Vector.map (removeIdxMap deletions) (fromList [0 .. size - 1])

remove deletions v = backpermute v (removeMap (Vector.length v) deletions)



 -- CUSTOM FUNCTIONS (INPUTS) 
dead :: Vector Person -> Vector Person
dead  = Vector.filter (\p -> state p == R)


sirWire :: WireP (Vector Person) (Vector Person)

sirWire = helper (Vector.replicate size personWire) where
    helper wires = mkPure $ 
                   \dt agents ->
                       let (persons, wires') = stepWiresP wires dt agents
                           newAgents = Vector.zipWith genPerson 
                                                    persons 
                                                    (Vector.zipWith PersonNetwork 
                                                           nbhs 
                                                           (Vector.map idx agents))
                           oldNetwork = neighboursAdj agents
                           nbhs = genNeighbours newAgents oldNetwork
                           -- PROCESS DEATH
                           deads = Vector.map (idx) . dead $ newAgents          
                           livePeople = remove deads persons
                           newSize = Vector.length livePeople
                           newNetwork = Vector.map (Vector.map (fromJust) . 
                                                    Vector.filter (isJust) . 
                                                    Vector.map (removeIdxMap deads)) 
                                                    (remove deads oldNetwork)
                           -- regenerate
                           liveAgents = Vector.zipWith genPerson
                                                    livePeople
                                                    (Vector.zipWith PersonNetwork
                                                           (genNeighbours liveAgents newNetwork)
                                                           (fromList [0..newSize-1]))
                           
                       in (Right liveAgents, helper (remove deads wires'))
          
         
          

{-
sirWire = proc agents -> do
            persons <- personsWire -< agents
            let newAgents = Vector.zipWith genPerson persons (Vector.zipWith 
                                                                    PersonNetwork 
                                                                    nbhs 
                                                                    (Vector.map idx agents))
                nbhs = genNeighbours newAgents sirNetwork
            returnA -< newAgents
-}
                          
                            



sir :: WireP () (Vector Person)
sir = loopWire startingAgents sirWire

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
wire n = forI n . arr show . (arr (Vector.map state) &&& arr (Vector.map (Vector.map (idx) . neighbours))) . sir

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
