{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Syntax
-- Copyright   :  (c) University of Saskatchewan 2013
-- 
-- Maintainer  :  ivan.vendrov@usask.ca
--
-- The abstract syntax for the Frabjous programming language,
-- with some non-compiler-specific utility functions
-----------------------------------------------------------------------------
module Syntax where 

import Data.Data (Typeable, Data)
import Data.Function (on)


type Name = String
-- TODO add position information for debugging to HaskellBlock
data HaskellBlock = HaskellBlock { contents :: String } deriving (Typeable, Data, Show, Eq, Ord) 
type Field = (Name, String)


-- a Program is a list of Blocks
data Program = Program {blocks :: [Block]} deriving Show

-- a Block can be a declaration or just a block of Haskell code
data Block = AgentDec { agentName :: Name,
                      fields :: [Field]}
         | VariableDec { varName :: Name,
                         code :: HaskellBlock}
         | PopulationDec { populationName :: Name,
                           agent :: Name,
                           removal :: Maybe HaskellBlock,
                           addition :: Maybe HaskellBlock}
         | NetworkDec { population1 :: (Name, Name),
                        population2 :: Maybe (Name, Name),
                        networkSpec :: HaskellBlock}
         | JustHaskell HaskellBlock
         
                        
           deriving (Eq, Ord, Typeable, Data, Show)

-- custom Ord implementation; the automatically-derived one tiebreaks by name, which I don't currently need
decType :: Block -> Int
decType (AgentDec _ _ ) = 0
decType (VariableDec _ _) = 1
decType (PopulationDec _ _ _ _) = 2
decType (NetworkDec _ _ _) = 3
decType (JustHaskell _) = 4


name :: Block -> Name
name (AgentDec n _) = n
name (VariableDec n _) = n
name (PopulationDec n _ _ _ ) = n