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

import Data.Map (Map)
import qualified Data.Map as Map


type Name = String
-- TODO add position information for debugging to HaskellBlock
data HaskellBlock = HaskellBlock { contents :: String } deriving (Show, Eq, Ord) 
type Field = (Name, String)

data Link = One | MaybeOne | Many deriving (Eq, Ord, Show)
readLink :: String -> Maybe Link
readLink "one" = Just One
readLink "many" = Just Many
readLink _ = Nothing

type NetworkAccess = (Link, Name)

data NetworkContext = Symmetric { population :: Name, 
                                  access :: NetworkAccess}
                    | Asymmetric { population :: Name, 
                                   access1, access2 :: NetworkAccess}
                    | Bipartite { population1, population2 :: Name, 
                                  access1, access2 :: NetworkAccess} 
                      deriving (Show)


data Agent = Agent {fields :: [Field]} deriving (Show)
data Attribute = Attribute {code :: HaskellBlock} deriving (Show)
data Population = Population {agent :: Name,
                              removal :: HaskellBlock,
                              addition :: HaskellBlock} deriving (Show)
data Network = Network {context :: NetworkContext,
                        networkSpec :: HaskellBlock} deriving (Show)

data Program = Program { agents :: Map Name Agent,
                         attributes :: Map Name Attribute,
                         populations :: Map Name Population,
                         networks :: Map Name Network,
                         otherCode :: [HaskellBlock]} deriving (Show)

emptyProgram = Program { agents = Map.empty,
                         attributes = Map.empty,
                         populations = Map.empty,
                         networks = Map.empty,
                         otherCode = []}

addAgent name agent program = program { agents = Map.insert name agent (agents program) }
addAttribute name attribute program = program { attributes = Map.insert name attribute (attributes program) }
addPopulation name population program = program {populations = Map.insert name population (populations program) }
addNetwork name network program = program { networks = Map.insert name network (networks program) }
addCode code program = program { otherCode = otherCode program ++ [code] }