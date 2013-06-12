-- OliDice.hs
-- Oliver Schneider
--
-- a library for dice rolling applications

-- module declaration
module OliDice
	(
		Die(..)
		, roll
		, d
	) where

import Random

--Data Definitions
data Die = D Int
	deriving(Show)
	

--Function Definitions

--External

-- roll
-- rolls a single die
roll:: Die -> IO Int
roll (D n) = do
				r <- randomRIO (1, n)
				return r
				

-- d
-- infix notation for rolling several dice
d :: Int -> Int -> IO Int
d a c = rollmany (consDice a c)


--Internal

-- rollmany
-- rolls a list of dice, returns their sum
rollmany:: [Die] -> IO Int
rollmany [] = return 0
rollmany (x:xs) = do
				r <- roll x
				s <- rollmany xs
				return (r + s)

-- consDice
-- makes a list of n dice of size s
consDice 0 _ = []
consDice (n+1) s = (D s):(consDice n s)
