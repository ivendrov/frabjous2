-----------------------------------------------------------------------------
-- |
-- Module      :  StandardLibrary
-- Copyright   :  (c) University of Saskatchewan 2013
-- 
-- Maintainer  :  ivan.vendrov@usask.ca
--
-- The Frabjous standard library
--------------------------------------------------------------------------
module StandardLibrary 
 (-- UTILITY FUNCTIONS
  clip,
  length,
  count,
  stepWire,
  -- RANDOM NUMBERS
  uniform,
  frequencies,
  draw,
  -- WIRE COMBINATORS
  -- | simple combinators
  constant,
  function, 
  -- | event combinators
  countEvents,
  (<|>),
  edge,
  changed,
  andThen,
  never,
  after,
  for,
  delay,

  -- | functions that take wires as parameters & analyze them somehow
  accumulate,
  onChange,

  -- | real numbers
  integrate,
  randomWalk,

  -- | randomness
  noise,
  poisson,
  rate,

  

  -- | switches 
  switch,
  statechart,


  -- NETWORKS
  randomNetwork,
  randomSymmetricNetwork,
  poissonRandomSymmetric,
  distanceBased,
  manhattan,
  euclidean,
  normed)
where

import InternalLibrary
import Prelude hiding ((.), id, length)
import Control.Monad.Random
import Data.Traversable as Traversable hiding (for)
import Control.Wire (arr, mkGen, mkPure, mkState, (.), Wire, WireM, EventM, LastException, 
                     (<|>), after, delay, for, andThen, edge,
                     changed)
import qualified Control.Wire as Wire
import Data.Monoid
import Data.Tuple (swap)
import Data.List hiding (length)
import qualified Data.List
import qualified Data.IntMap as IntMap


-- -1. UTILITY FUNCTIONS
-- | the function 'clip' keeps its value in the given closed interval
clip (a,b) x = if x < a then a
               else if x > b then b
               else x

-- | generalizes length to any numeric type, allowing one to elide 'fromIntegral' in code
length :: (Num n) => [a] -> n
length = fromIntegral . Data.List.length

-- | counts the number of elements that satisfy the given predicate
count :: Num n => (a -> Bool) -> [a] -> n
count pred = length . filter pred


-- 0. RANDOM NUMBERS 

uniform :: MonadRandom m => (Double, Double) -> m Double
uniform = getRandomR 

frequencies :: MonadRandom m => [(a, Double)] -> m a
frequencies probs = 
    let (vals, pdf) = unzip probs
        cdf = scanl1 (+) pdf
    in do
      p <- getRandom
      let Just idx = findIndex (>= p) cdf
      return (vals !! idx)

draw :: MonadRandom m => Int -> m a -> m [a]
draw n rand = Traversable.sequence (replicate n rand)
      
  
stepWire :: Monad m => Wire.WireM m a b -> Wire.Time -> a -> m (Either LastException b, Wire.WireM m a b)
stepWire = Wire.stepWire
  


-- 1. WIRE COMBINATORS

constant :: (Monad m) => b -> Wire e m a b
constant = Wire.pure 

function :: (Monad m) => (a -> b) -> Wire e m a b
function = Wire.arr

never :: (Monad m, Monoid e) => Wire e m a b
never = Wire.empty

countEvents :: (Monad m, Monoid e) => Wire e m a Int
countEvents = internalState (const (+1)) 0




-- | draws a value from the given distribution at every timestep
noise :: MonadRandom m => m a -> Wire LastException m input a
noise distribution = mkGen $ \_ _ -> do
                       val <- distribution
                       return (Right val, noise distribution)



-- | integrates its input with respect to time
integrate :: Monad m => Double -> Wire e m Double Double
integrate = internalStateT (\dt x -> (+ dt*x))

-- | performs a 1D random walk at the velocity specified by the given distribution
randomWalk init distribution = integrate init . noise distribution

-- | (TODO REFACTOR?) internalStateT applies the given transition function
-- | to the timestep, input, and state at every step, then outputs the new state
internalStateT transition init = mkState init $ \dt (x, state) -> 
                                 let newState = transition dt x state
                                 in newState `seq` (Right newState, newState)

-- | internalState applies the given transition function 
-- | to its input and state at every step, then outputs the state
internalState transition init = internalStateT (const transition) init




-- a wire that takes as input a rate, and produces a unit value with a rate
-- corresponding to the given time, minus inaccuracy with multiple occurrences in a 
-- single time interval
rate :: MonadRandom m  => Wire LastException m Double ()
rate = mkGen $
           \dt lambda -> do 
             e <- getRandom
             return (if (e < 1 - exp (-dt * lambda)) then Right () else Left mempty, rate)


-- models a poisson process with rate parameter lambda
poisson :: MonadRandom m => Double -> Wire LastException m a Int
poisson lambda = accumulate (const . (+1)) 0 (rate . constant lambda)


-----------------------------------------------------
-- Functions that take wires as parameters 
-- and analyze them somehow - ('higher order' wires)
-----------------------------------------------------
-- | accumulates the output of a signal function by the given combining function, with the given starting state
accumulate :: Monad m => (b -> a -> b) -> b -> Wire e m c a -> Wire e m c b
accumulate binop init wire = Wire.hold init (Wire.accum binop init . wire)   


-- | produces when the argument wire changes output
onChange :: (Eq b, Monad m) => WireM m a b -> EventM m a
onChange wire = on (changed . wire) 

-- | produces when the argument wire produces
on :: Monad m => WireM m a b -> EventM m a
on wire = mkGen $ \dt x -> do
                  (output, wire') <- stepWire wire dt x
                  let output' = case output of 
                                  Right _ -> Right x
                                  Left e -> Left e
                  return (output', on wire')
                


-- | whenever produces produces a wire, that wire is switched into (and starts at time 0)
-- | (difference from Netwire is that there's no initial wire)
switch producer = Wire.switch producer never



-- statechart :: Wire a b -> Wire a (Wire a b) -> Wire a b
-- statechart state transitions is a wire whose internal state will be the most recent
-- value produced by transitions; and which is refreshed every time its internal state changes
statechart state transitions = switch (transitions . onChange state) <|> state

-- 2. NETWORKS

--  A) Library functions for creation 
pairs xs = [(u, v) | u <- xs, v <- xs, u < v]


randomNetwork, randomSymmetricNetwork :: (MonadRandom m) => Double -> [Int] -> [Int] -> m ManyToMany
-- randomNetwork rng fraction size = a random network with the given integer vertices. 
-- each directed edge has a given probability of existing

randomNetwork fraction vertices1 vertices2 = do 
    randoms <- getRandoms 
    let edges = map snd . filter ((<fraction) . fst) . zip randoms $ 
            [(u,v) | u <- vertices1, 
                     v <- vertices2]
    return $ fromEdges vertices1 vertices2 edges

-- randomSymmetricNetwork is like a random network but for a single population 
-- each UNDIRECTED edge has a "fraction" probability of existing
randomSymmetricNetwork fraction vertices _ = do
  randoms <- getRandoms
  let edges' = map snd . filter ((<fraction) . fst) . zip randoms $ 
               [(u,v) | u <- vertices, 
                v <- vertices, u < v]
      edges = edges' ++ map swap edges'
  return $ fromEdges vertices vertices edges

--  B) dynamic networks
poissonRandomSymmetric :: Double ->
                          (model -> ReactiveOutput a) ->                 
                              ModelWire model ManyToMany
poissonRandomSymmetric prob extractPop = helper where
  helper = mkGen $ \dt model -> do
             let v1 = indices . extractPop $ model
             network <- randomSymmetricNetwork prob v1 v1
             return (Right network, helper)

distanceBased :: (a -> a -> Double) -> 
                 (Double -> Bool) -> 
                (model -> ReactiveOutput a) -> 
                    ModelWire model ManyToMany
distanceBased d pred extractPop = function helper where
    helper model = let pop = collection . extractPop $ model
                       indices = IntMap.keys pop
                       indexPairs = pairs indices
                       withinDistance (i1, i2) = pred $ d (pop IntMap.! i1) (pop IntMap.! i2) 
                   in fromEdges indices indices (filter withinDistance indexPairs)

euclidean :: [a -> Double] -> a -> a -> Double
euclidean = normed 2


manhattan :: Num n => [a -> n] -> a -> a -> n
manhattan accessors p1 p2 = sum . map abs $ diffs accessors p1 p2

diffs accessors p1 p2 = 
    let coords1 = map ($ p1) accessors
        coords2 = map ($ p2) accessors
    in zipWith (-) coords1 coords2
    

normed :: Double -> [a -> Double] -> a -> a -> Double
normed p accessors p1 p2 = norm (diffs accessors p1 p2)
    where norm diffs = sum (map (**p) diffs) ** (1/p)


                       
                       
                       


             





{-
-- sample dynamic network specifications : 

-- a. makes "v" a neighbour of "u" iff (pred u v)
predicateNetwork :: (a -> a -> Bool) -> Vector a -> Vector (Vector a)
predicateNetwork pred agents = map (\p -> filter (pred p) agents) agents 

-- b . makes u and v neighbours iff label u = label v
equivalenceClass :: (Eq b) => (a :-> b) -> Vector a -> Vector (Vector a)
equivalenceClass label = predicateNetwork ((==) `on` (get label))
    

-- c. makes person i belong to neighbourhood j of n iff i % n = j
evenlyDistribute people nbhds = map (nbhds!) $ map (`mod` n) (fromList [0 .. nPeople-1]) where
    n = length nbhds
    nPeople = length people

-}