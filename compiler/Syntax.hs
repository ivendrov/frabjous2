{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Syntax
-- Copyright   :  (c) University of Saskatchewan 2013
-- License     :  BSD-style 
-- 
-- Maintainer  :  ivan.vendrov@usask.ca
-- Stability   :  experimental
-- Portability :  portable
--
-- The abstract syntax for the Frabjous programming language,
-- with some non-compiler-specific utility functions
-----------------------------------------------------------------------------
module Syntax where 

import Data.Data (Typeable, Data)
import Data.Function (on)


type Name = String
type HaskellString = String
type Field = (Name, HaskellString)

data Program = Program {preamble :: HaskellString,
                        decs :: [Dec]}

data Dec = AgentDec { agentName :: Name,
                      fields :: [Field]}
         | VariableDec { varName :: Name,
                         code :: HaskellString}
         | PopulationDec { populationName :: Name,
                           agent :: Name,
                           removal :: Maybe HaskellString,
                           addition :: Maybe HaskellString}
         | NetworkDec { population1 :: (Name, Name),
                        population2 :: Maybe (Name, Name),
                        networkSpec :: HaskellString }
         
                        
           deriving (Eq, Typeable, Data)

-- custom Ord implementation; the automatically-derived one tiebreaks by name, which I don't currently need
decType :: Dec -> Int
decType (AgentDec _ _ ) = 0
decType (VariableDec _ _) = 1
decType (PopulationDec _ _ _ _) = 2
decType (NetworkDec _ _ _) = 3

instance Ord Dec where
    compare = compare `on` decType



name :: Dec -> Name
name (AgentDec n _) = n
name (VariableDec n _) = n
name (PopulationDec n _ _ _ ) = n