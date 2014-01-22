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
                           removal :: HaskellBlock,
                           addition :: HaskellBlock}
         | NetworkDec { population1 :: (Name, Name),
                        population2 :: Maybe (Name, Name),
                        networkSpec :: HaskellBlock}
         | JustHaskell HaskellBlock
         
                        
           deriving (Eq, Ord, Typeable, Data, Show)