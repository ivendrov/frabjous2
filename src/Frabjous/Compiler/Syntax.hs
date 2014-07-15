{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Frabjous.Compiler.Syntax
-- Copyright   :  (c) University of Saskatchewan 2013
-- 
-- Maintainer  :  ivan.vendrov@usask.ca
--
-- The abstract syntax for the Frabjous programming language,
-- with some non-compiler-specific utility functions
-----------------------------------------------------------------------------
module Frabjous.Compiler.Syntax where 

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


data Agent = Agent {attributes :: [Attribute]} deriving (Show)
data Attribute = Attribute {name :: String, annotation :: String, code :: Maybe HaskellBlock} deriving (Show)
data Population = Population {agent :: Name,
                              removal :: HaskellBlock,
                              addition :: HaskellBlock} deriving (Show)
data Network = Network {context :: NetworkContext,
                        networkSpec :: HaskellBlock} deriving (Show)

data Program = Program { agents :: Map Name Agent,
                         populations :: Map Name Population,
                         networks :: Map Name Network,
                         statistics :: Map Name HaskellBlock,
                         initial :: Map Name HaskellBlock,
                         otherCode :: [HaskellBlock]} deriving (Show)

emptyProgram = Program { agents = Map.empty,
                         populations = Map.empty,
                         networks = Map.empty,
                         statistics = Map.empty,
                         initial = Map.empty,
                         otherCode = []}

addAgent name agent program = program { agents = Map.insert name agent (agents program) }
addPopulation name population program = program {populations = Map.insert name population (populations program) }
addNetwork name network program = program { networks = Map.insert name network (networks program) }
addStatistic name statistic program = program { statistics = Map.insert name statistic (statistics program) }
addInitial initials program = if Map.null (initial program)
                              then program { initial = Map.fromList initials }
                              else error "initial state specified twice"
addCode code program = program { otherCode = otherCode program ++ [code] }