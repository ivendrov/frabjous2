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
import Control.Monad.Identity (Identity)
import Control.Arrow
import Control.Wire
import System.Random (mkStdGen)
import Text.Printf
import qualified Data.List as List
import Data.Vector
import Data.Ord
import Data.Monoid
import Data.Maybe (isJust, fromJust)
import Data.Function (on)
import Data.Graph
import Data.Tuple (swap)
import Data.Label
import Data.Typeable

import Control.Wire.Classes

-- FRABJOUS STANDARD LIBRARY

----------------
-- A. Utility --
----------------
type AdjList = Vector (Vector Int)


--  1) Removal

-- removeIdxMap deletions i is the new index of the element 
-- that used to be at the ith position before the elements at indices "deletions" were removed
-- deletions must be in increasing order
removeIdxMap :: Vector Int -> (Int -> Maybe Int)
removeIdxMap deletions i = if (any (==i) deletions) then Nothing
                           else Just $ i - length (takeWhile (<i) deletions)

removeMap size deletions = findIndices (isJust) $ 
                           map (removeIdxMap deletions) (fromList [0 .. size - 1])

remove deletions v = backpermute v (removeMap (length v) deletions)
regenIndices v = map (fromJust) . filter (isJust) . map (removeIdxMap v)



--  2) Wire Combinators
never :: (Monad m, Monoid e) => Wire e m a b
never = Control.Wire.empty


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

-- rSwitch switches between wires based on the given discriminator function "computeWire"
-- currently, every time its current wire changes output, it plugs the output into computeWire
-- to determine the wire to switch to.
-- TODO: decompose the "every time wire changes" and the "select wire based on a function" 
--       into two separate combinators. The second combinator could be used to implement
--       phase shifts in other variables (e.g. income calculation as child vs adult)
rSwitch computeWire initialState = helper' initialState (computeWire initialState) where
    helper' state currentWire = 
        mkGen $ \dt x -> do
          (output, currentWire') <- stepWire currentWire dt x
          case output of 
            Left _ -> error "status wire should never inhibit"
            Right newState -> 
                return (output, 
                        helper' 
                          newState           
                          (if (newState == state) then currentWire' else computeWire newState))

-- Parallel wire operator (just pure wires for now)
par :: Vector (WireP a b) -> WireP (Vector a) (Vector b)
par wires = 
    mkPure $ \dt inputs -> 
        let (value, wires') = stepWiresP wires dt inputs
        in (Right value, par wires')

-- steps each wire by the given timestep
stepWiresP wires dt inputs = 
    let dts = replicate (length wires) dt 
        (values, wires') = unzip $ zipWith3 stepWireP wires dts inputs 
        right x = case x of 
                    Right y -> y
                    Left _ -> error "cannot extract right from a Left contructor"
        isRight x = case x of
                      Right _ -> True
                      Left (Last _) -> False
        value = if (all isRight values) 
                then map right values
                else error "wires in stepWiresP must produce"  -- if a single wire doesn't produce, nothing produces
        in (value, wires')
 

-- B. Agents and Populations

class Agent a where
    idx :: a :-> Int
    localChangeWire :: WireP a a

fclabels [d|
             data PopulationState a = PopulationState { agentWires :: Vector (WireP a a),
                                                        removal :: WireP (Vector a) (Vector a), --death
                                                        addition :: WireP (Vector a) (Vector a)} --birth

             data PopulationOutput a = PopulationOutput { agents :: Vector a,
                                                          removedIndices :: Vector Int}


          |]

-- evolve the local properties of the population (unrelated to networks)
-- i.e modify each agent locally, then apply death and birth (UNIMPLEMENTED)
evolvePopulation :: (Agent a) => PopulationState a -> WireP (Vector a) (PopulationOutput a)
evolvePopulation (PopulationState agentWires removal addition) = 
    mkPure $ \dt agents ->
        let (agents', agentWires') = stepWiresP agentWires dt agents
            (deadAgents, removal') = stepWireP removal dt agents'
            (newAgents, addition') = stepWireP addition dt agents'
        in
           let removedIndices = case deadAgents of 
                                  Left _ -> fromList []
                                  Right deads -> map (get idx) deads
               agentsAfterRemoval = remove removedIndices agents'
               newborns = case newAgents of
                            Left _ -> fromList []
                            Right as -> as
              
               outputAgents = zipWith (set idx) (fromList [0 .. length agentsAfterRemoval - 1]) agentsAfterRemoval
               newWires = (remove removedIndices agentWires')
               output = PopulationOutput outputAgents removedIndices
                           
           in (Right output, evolvePopulation (PopulationState newWires removal' addition'))


    

-- C. Networks

data NetworkLabel a b = One (a :-> b) 
                      | MaybeOne (a :-> Maybe b)
                      | Many (a :-> Vector b)


-- networkRemove as lbl bs deadBs
-- removes all indices in deadB's from as' adjacency lists and vice versa,
-- with the adjacency lists specified by the labels

{- 
networkRemove :: (NetworkLabel a b, NetworkLabel b a) -> 
                 (Vector a, Vector b) -> 
                 (Vector Int, Vector Int) -> 
                     (Vector a, Vector b)
networkRemove (labelA, labelB) (as, bs) (deadAs, deadBs) = 
    (newAs, newBs) where
        lA = label labelA
        lB = label labelB
       
       -} 

{-
class Network n where
    networkRemove :: Vector Int -> Vector Int -> n -> n
   -- networkAdd ::  TODO

data ManyToMany = ManyToMany {adj1, adj2 :: Vector (Vector Int)}
data OneToMany = OneToMany {children :: Vector (Vector Int),
                            parent :: Vector (Int)}

instance Network ManyToMany where
    networkRemove v1 v2 (ManyToMany a1 a2) = ManyToMany newA1 newA2 where
                         newA1 = map (regenIndices v2) (remove v1 a1)
                         newA2 = map (regenIndices v1) (remove v2 a2)
                         

instance Network OneToMany where 
    networkRemove v1 v2 (OneToMany c p) = OneToMany newC newP where
                           newC = map (regenIndices v2) (remove v1 c)
                           newP = map (fromJust . removeIdxMap v1) (remove v2 p) 
                           -- will throw exception if a parent dies without all the children dying 

-}

-- networks <-> agent populations 

genNeighbours :: Vector a -> AdjList -> Vector (Vector a)
genNeighbours as adj = map (map (as !)) adj

--neighboursAdj :: (Agent a) => Vector a -> NetworkLabel a b -> Network
--neighboursAdj = map (map (get a) . (get neighbours)) 
                                                       
             




-- USER-SPECIFIED DECLARATIONS
data State = S | I | R deriving (Eq, Show)

fclabels [d|
             data Person = Person { income :: Double,
                                    state :: State,
                                    neighbours :: Vector Person,
                                    nbhd :: Nbhd,
                                    idxPerson :: Int} deriving Typeable-- automatically filled in

             data Nbhd = Nbhd { avgIncome :: Double,
                                residents :: Vector Person,
                                idxNbhd :: Int} deriving Typeable-- automatically filled in

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

--never = Control.Wire.empty -- event that never triggers 

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

randomNetwork :: RandomGen r => r -> Double -> Int -> AdjList
-- randomNetwork rng fraction size = a random network with the given number of nodes. 
--   each pair of nodes has a "fraction" probability of an edge

randomNetwork rng fraction size =  fromList . List.map (fromList) $ adjList where
    edges' = List.map snd . List.filter ((<fraction) . fst) . List.zip (randoms rng) $ 
            [(u,v) | u <- [0 .. size - 1], 
                     v <- [u+1 .. size -1]]
    edges = edges' List.++ (List.map swap edges')
    adjList = List.map (\i -> List.map (snd) . List.filter (\edge -> fst edge == i) $ edges) [0 .. size -1] 


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
    addition = never {-arr (\people -> 
                        if (length people < 1) then fromList []
                        else 
                            let 
                                newborn = set idxPerson(length people) . set neighbours(fromList [parent]) $ parent
                                parent = people ! 0
                            in
                              fromList [newborn])-}

averageIncome = sum . map (get income)

saskatoonWire = arr (\sask -> set avgIncome (averageIncome . get residents $ sask) sask)
            
initialNbhds = PopulationState (fromList [saskatoonWire,saskatoonWire]) never never

-- sample dynamic network specifications : 

-- a. makes two people neighbours iff their states are equal
sameStateNetwork people = map (\p -> filter (sameState p) people) people where 
    sameState p1 p2 = get state p1 == get state p2

-- b. makes person i belong to neighbourhood j of n iff i % n = j
evenlyDistribute people nbhds = map (nbhds!) $ map (`mod` n) (fromList [0 .. nPeople-1]) where
    n = length nbhds
    nPeople = length people

-- CODE BELOW IS BEING REFACTORED 

tieMany agents adj = map (map (agents !)) adj
tieOne agents adj = map (agents !) adj
getIdxMany :: (Agent a) => Vector (Vector a) -> Vector (Vector Int)
getIdxMany = map (map (get idx)) 
getIdxOne :: (Agent a) => Vector a -> Vector Int
getIdxOne = map (get idx)
dieMany deads =  map (regenIndices deads)
dieOne deads = map (fromJust . removeIdxMap deads) -- throws exception if parent dies without all children dying




-- computeNetworkSelf computeAdj label as 
-- recomputes the network between agents in as specified by (map (get labelA) as)
-- using computeAdj, and returns the resulting calculation
computeNetworkSelfManyMany :: (Agent a) => 
                              (Vector a -> Vector (Vector a)) ->
                              (a :-> Vector a) ->
                              (Vector a -> Vector a)
computeNetworkSelfManyMany computeAdj label as = 
    newAs where
        newAs = zipWith (set label) newNeighbours as
        newNeighbours = tieMany newAs newAdj
        newAdj = getIdxMany $ computeAdj as
 






-- computeNetwork computeAdj (labelA, labelB) (as, bs)
-- recomputes the bipartite network between agents in as and bs specified by 
-- (map (get labelA) as, map (get labelB) bs), 
-- using the given computeAdj to get the first and taking its transpose to get the second 
-- and returns the resulting populations

-- many to one version (many a's are mapped to a single b)
computeNetworkManyOne :: (Agent a, Agent b) => 
                         (Vector a -> Vector b -> Vector b) -> 
                         (a :-> b, b :-> Vector a) ->
                         (Vector a, Vector b) -> 
                             (Vector a, Vector b)
computeNetworkManyOne computeAdj (labelA, labelB) (as, bs) = 
    (newAs, newBs) where 
        newAs =  zipWith (set labelA) newANeighbours as 
        newANeighbours = tieOne newBs newAAdj
        newBs = zipWith (set labelB) newBNeighbours bs 
        newBNeighbours = tieMany newAs newBAdj
        newAAdj = getIdxOne $ computeAdj as bs
        newBAdj = networkTransposeOneMany newAAdj where
                   networkTransposeOneMany :: Vector Int -> Vector (Vector Int)
                   networkTransposeOneMany vi = map residents (fromList [0.. length vi - 1]) where
                                                  residents i = findIndices (==i) vi -- TODO optimize
                         
       
processDeath :: (Agent a, Agent b, Typeable setB, Typeable b) =>
                    (a :-> setB) -> Vector b -> Vector Int -> Vector a -> Vector a 
processDeath label newBs deadBs as = 
    case (gcast label) of 
      Just label -> -- case label :: a -> Vector b
          zipWith (set label) newNeighbours as where
                     newNeighbours = tieMany newBs newAdj
                     newAdj = dieMany deadBs oldAdj
                     oldAdj = getIdxMany oldNeighbours
                     oldNeighbours = map (get label) as 
      Nothing -> case (gcast label) of
                   Just label -> -- case label :: a -> b
                        zipWith (set label) newNeighbours as where
                                    newNeighbours = tieOne newBs newAdj
                                    newAdj = dieOne deadBs oldAdj
                                    oldAdj = getIdxOne oldNeighbours
                                    oldNeighbours = map (get label) as
                   Nothing -> error "case not implemented yet"


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
          newPeople2 = computeNetworkSelfManyMany 
                       sameStateNetwork 
                       neighbours
                       newPeople
          (newPeople3, newNbhds2) = computeNetworkManyOne
                                    evenlyDistribute 
                                    (nbhd, residents) 
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
startingNeighbours = genNeighbours startingPeople sirNetwork
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