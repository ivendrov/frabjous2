-----------------------------------------------------------------------------
-- |
-- Module      :  StandardLibrary
-- Copyright   :  (c) University of Saskatchewan 2013
-- 
-- Maintainer  :  ivan.vendrov@usask.ca
--
-- The Frabjous standard library
--------------------------------------------------------------------------
module Frabjous.StdLib
 (Real,
 
  -- * Utility Functions
  clip,
  length,
  count,
  fraction,
  average,
  stepWire,
  
  -- * Random Numbers
  uniform,
  frequencies,
  draw,
  
  -- * Wire Combinators
  -- ** simple combinators
  constant,
  function, 
  -- ** event combinators
  (<|>),
  edge,
  changed,
  andThen,
  never,
  after,
  for,
  delay,

  -- ** real numbers
  integrate,
  randomWalk,

  -- ** randomness
  noise,
  poisson,
  countingPoisson,
  rate,

  -- ** functions that take wires as parameters & analyze them somehow
  accumulate,
  countEvents,
  onChange,
  internalStateT,
  internalState,

  -- ** switches 
  switch,
  stateDiagram,
  
  -- * Networks
  -- ** static networks
  emptyNetwork,
  randomNetwork,
  predicate,
  gridWithDiags,
  {-
  randomSymmetricNetwork,
  poissonSymmetricNetwork,
  
  
  -- ** dynamic networks
  memorylessSymmetric,
  randomSymmetric,
  poissonRandomSymmetric,
  distanceBased,
  
  -}

  -- ** auxiliary functions
  manhattan,
  euclidean,
  normed)
where

import Frabjous.StdLib.Internal hiding (edge)
import qualified Frabjous.Compiler.Syntax as Syntax
import Prelude hiding ((.), id, length, Real)
import Control.Monad.Random hiding (uniform)
import Data.Traversable as Traversable hiding (for)
import Control.Wire (mkGen, mkState, (.), Wire, WireM, EventM, LastException, 
                     (<|>), after, delay, for, andThen, edge,
                     changed)
import qualified Control.Wire as Wire
import Data.Monoid
import Data.Tuple (swap)
import Data.List hiding (length)
import qualified Data.List
import qualified Data.IntMap as IntMap
type Real = Double

-- -1. UTILITY FUNCTIONS
-- | the function 'clip' keeps its value in the given closed interval
clip (a,b) x = if x < a then a
               else if x > b then b
               else x

-- | generalizes length to any numeric type, allowing one to elide 'fromIntegral' in code
length :: (Num n) => [a] -> n
length = fromIntegral . Data.List.length

-- | counts the number of elements that satisfy the given predicate
count :: (a -> Bool) -> [a] -> Integer
count pred = length . filter pred

-- | determines the fraction of elements satisfying the given predicate
fraction :: (a -> Bool) -> [a] -> Double
fraction pred l = fromIntegral (count pred l) / length l

-- | finds the average of a list of numbers
average :: [Double] -> Double
average l = sum l / length l


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
internalStateT :: 
        (Double -> a -> s -> s) -- ^ the transition function
         -> s -- ^ the initial state
         -> Wire e m a s
internalStateT transition init = mkState init $ \dt (x, state) -> 
                                 let newState = transition dt x state
                                 in newState `seq` (Right newState, newState)

-- | internalState applies the given transition function 
-- | to its input and state at every step, then outputs the state
internalState transition init = internalStateT (const transition) init




-- nonhomogenous poisson process
rate :: MonadRandom m  => Wire LastException m Double ()
rate = mkGen $
           \dt lambda -> do 
             e <- getRandom
             return (if e < 1 - exp (-dt * lambda) then Right () else Left mempty, rate)

-- a poisson process with rate parameter lambda
poisson :: MonadRandom m => Double -> Wire LastException m a ()
poisson lambda = rate . constant lambda

-- a counting poisson process
countingPoisson :: MonadRandom m => Double -> Wire LastException m a Int
countingPoisson lambda = countEvents (poisson lambda)



-----------------------------------------------------
-- Functions that take wires as parameters 
-- and analyze them somehow - ('higher order' wires)
-----------------------------------------------------
-- | orElse is a synonym for <|> (acts like the first wire if it produces; otherwise, like the second)
orElse :: Monad m => WireM m a b -> WireM m a b -> WireM m a b
orElse = (<|>)

-- | accumulates the output of a signal function by the given combining function, with the given starting state
accumulate :: Monad m => (b -> a -> b) -> b -> Wire e m c a -> Wire e m c b
accumulate binop init wire = Wire.hold init (Wire.accum binop init . wire)   


-- | count the number of events of an argument wire
countEvents :: Monad m => Wire e m a b -> Wire e m a Int
countEvents wire = accumulate (+) 0 (1 . wire)

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
                


-- | whenever producer produces a wire, that wire is switched into (and starts at time 0)
-- | (difference from Netwire is that there's no initial wire)
switch producer = Wire.switch producer never



-- statechart :: Wire a b -> Wire a (Wire a b) -> Wire a b
-- statechart state transitions is a wire whose internal state will be the most recent
-- value produced by transitions; and which is refreshed every time its internal state changes
stateDiagram state transitions =  switch (transitions . onChange state) `orElse` state

-- 2. NETWORKS

--  A) Library functions for creation 
pairs xs = [(u, v) | u <- xs, v <- xs, u < v]
symmetrify edges = edges ++ map swap edges


emptyNetwork :: (MonadRandom m) => [Int] -> [Int] -> m (Network e)
emptyNetwork v1 v2 = return $ Network (Syntax.Many, Syntax.Many) v1 v2 []

randomNetwork :: (MonadRandom m) => Double -> [Int] -> [Int] -> m (Network ())
-- randomNetwork rng fraction size = a random network with the given integer vertices. 
-- each directed edge has a given probability of existing
randomNetwork fraction vertices1 vertices2 = do 
    randoms <- getRandoms 
    let edges = map snd . filter ((<fraction) . fst) . zip randoms $ 
            [(u,v) | u <- vertices1, 
                     v <- vertices2]
    return $ Network (Syntax.Many, Syntax.Many) vertices1 vertices2 (zipWith Edge edges (repeat ()))
    
    
predicate :: (a -> a -> Bool) -> NetworkGenerator a () -- TODO generalize to any edge type
predicate connected pop _ = 
    let vertices = IntMap.toList pop
        edges = [(i1, i2) | (i1, v1) <- vertices, (i2, v2) <- vertices, i1 < i2, connected v1 v2]
        indices = IntMap.keys pop
    in return $ Network (Syntax.Many, Syntax.Many) indices indices (zipWith Edge edges (repeat ()))
    
gridWithDiags :: Int -> [a -> Int] -> NetworkGenerator a () -- TODO generalize to any edge type
gridWithDiags resolution coords = predicate connected where
    connected a1 a2 = let x = map ($a1) coords
                          y = map ($a2) coords
                          diffs = map abs $ zipWith (-) x y
                      in all (<= resolution) diffs

{-
-- poissonSymmetricNetwork is like a random network but for a single population 
-- each UNDIRECTED edge has a "fraction" probability of existing
poissonSymmetricNetwork fraction vertices _ = do
  randoms <- getRandoms
  let edges = map snd . filter ((<fraction) . fst) . zip randoms $ pairs vertices     
  return $ fromEdges vertices vertices (symmetrify edges)




-- | creates a random network with each link probability calculated using the 
-- | binary function prob
randomSymmetricNetwork :: ((a,a) -> Double) -> NetworkGenerator a
randomSymmetricNetwork prob pop _ = do
  randoms <- getRandoms
  let vertices = IntMap.toList pop
      edges' = map (snd . snd) . filter (\(r, (vs, _)) -> r < prob vs) . zip randoms $ 
               [((v1, v2), (i1, i2)) | (i1, v1) <- vertices, 
                (i2, v2) <- vertices, i1 < i2]
      edges = edges' ++ map swap edges'
      indices = map fst vertices
  return $ fromEdges indices indices edges





  

--  B) dynamic networks

-- converts a static network generator into a dynamic one by just applying it every step
memorylessSymmetric :: NetworkGenerator a -> 
                       (model -> ReactiveOutput a) -> ModelWire model ManyToMany
memorylessSymmetric staticCreator extractPop = helper where
    helper = mkGen $ \dt model -> do
                   let v1 = collection . extractPop $ model
                   network <- staticCreator v1 v1
                   return (Right network, helper)

-- | have links between agents based on the given probability function
randomSymmetric :: ((a,a) -> Double) -> (model -> ReactiveOutput a) ->
                   ModelWire model ManyToMany
randomSymmetric prob = memorylessSymmetric (randomSymmetricNetwork prob)
 
poissonRandomSymmetric :: Double ->
                          (model -> ReactiveOutput a) ->                 
                              ModelWire model ManyToMany
poissonRandomSymmetric prob = randomSymmetric (const prob)


predicateDynamic pred = memorylessSymmetric (predicate pred)
{-
predicate :: (a -> a -> Bool) -> (model -> ReactiveOutput a) -> ModelWire model ManyToMany
predicate connected extractPop = function helper where
    helper model = let pop = collection . extractPop $ model
                       indices = IntMap.keys pop
                       indexPairs = pairs indices
                       withinDistance (i1, i2) = connected (pop IntMap.! i1) (pop IntMap.! i2) 
                   in fromEdges indices indices (symmetrify $ filter withinDistance indexPairs)
-}


distanceBased :: Num n => (a -> a -> n) -> 
                 (n -> Bool) -> 
                (model -> ReactiveOutput a) -> 
                    ModelWire model ManyToMany
distanceBased d pred = predicateDynamic (\ a b -> pred $ d a b)

-}

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