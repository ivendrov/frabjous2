{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Main where

import Control.Monad hiding (when)
import Control.Monad.Identity (Identity)
import Control.Arrow
import Control.Wire hiding (avg)
import System.Random (mkStdGen)
import Text.Printf
import Prelude hiding ( (.), id)
import Data.List as List
import Data.Vector as Vector


type Income = Double
type Vector2D a = Vector (Vector a)
type Health = Double
-- SIM DEFINITION --
baseIncomes, startIncomes :: Vector Double
baseIncomes = fromList [0.8, 1.2, 0.5, 1.5]
startIncomes = fromList [1,2,3,4]


-- NETWORK DEFINITION -- 
numNbhd = 2
numSims = 4

-- a containment is a bipartite graph between two sets of agents, A and B. Every agent in A 
-- is matched up to (contained in) a single agent in B.
-- so adjList1 !! n is the containing agent for the nth agent in A
-- and adjList2 !! n is the set of agents contained in the nth agent of B 
data ContainmentNetwork = CNetwork { adjList1 :: Vector Int, adjList2 :: Vector2D Int} deriving Show

-- generate a containment network from just the first adjList and a number of containing agents (constant, presumably)
networkFromAdjList1 :: Vector Int -> Int -> ContainmentNetwork
networkFromAdjList1 adjList numContainers = CNetwork adjList 
                                                  (generate 
                                                   numContainers
                                                   (`Vector.elemIndices` adjList))

nbhdNetwork = networkFromAdjList1 (fromList [0,0,1,1]) numNbhd

nbhd = adjList1 nbhdNetwork
residents = adjList2 nbhdNetwork


-- BASIC MODEL EQUATIONS (user-specified) -- 

calcIncome :: Income -> Income -> Income
calcIncome baseIncome nbhdAvgIncome = nbhdAvgIncome * baseIncome

calcHealth :: Income -> Health
calcHealth income  =  income * 10

-- UTILITY METHODS / CONVERSIONS (should be handled internally by Frabjous||) --
average :: Vector Double -> Double
average l = (Vector.sum l) / fromIntegral (Vector.length l)

calcIncomes :: Vector Income -> Vector Income
calcIncomes avgNbhdIncomes = Vector.zipWith calcIncome baseIncomes (backpermute avgNbhdIncomes nbhd)

calcAverages :: Vector Income -> Vector Income
calcAverages incomes = Vector.map (average . backpermute incomes) residents

calcHealths :: Vector Income -> Vector Health
calcHealths = Vector.map calcHealth


-- MAIN LOGIC (FRP) - should be handled internally -- 

info = proc _ -> do 
   rec incomes <- delay startIncomes . arr calcIncomes -< avgNbhdIncomes
       avgNbhdIncomes <-  arr calcAverages -<  incomes
       healths <- arr calcHealths -< incomes
   returnA -< (avgNbhdIncomes, incomes, healths)

wire :: WireP () String
wire = forI 20 . arr show . info


control whenInhibited whenProduced wire = loop wire clockSession where
    loop w' session' = do
      (mx, w, session) <- stepSessionP w' session' ()
      case mx of 
        Left ex -> whenInhibited ex
        Right x -> do whenProduced x
                      loop w session
main = control return (putStrLn) $ wire


{-
	networkFromAdjList adj = Network (fromList (Z:.m) cadg) (fromList (Z:.n) $ List.map (length) adj)
		where cadg = concat adj
		      m = length cadg -- number of edges / connections
		      n = length adj -- number of vertices / agents

-- a network is an flattened adjacency list, and an array storing 
-- the number of connections for each agent
data Network = Network { adjList :: (Array DIM1 Int),
			 segments :: (Array DIM1 Int)}
    deriving (Show)
-}