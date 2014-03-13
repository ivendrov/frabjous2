{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  CodeGen
-- Copyright   :  (c) University of Saskatchewan 2013
-- 
-- Maintainer  :  ivan.vendrov@usask.ca
--
-- The Code Generator for the Frabjous programming language
-- (given a Frabjous model, generates the Haskell code for it, minus header and footer info)
--------------------------------------------------------------------------
module CodeGen (generateCode) where 



import Data.Maybe (mapMaybe, fromMaybe, fromJust)
import Data.List
import Data.Function (on)
import Text.Printf (printf)
import Data.Char (toUpper)
import Data.Map (Map, (!))
import qualified Data.Map as Map

import Syntax
import qualified Transform

-- capitalizes the first letter of a given attribute
capitalize [] = []
capitalize (h : t) = toUpper h : t


generateCode :: Program -> String
generateCode (Program agents attributes populations networks othercode) =         
        unlines [unlines (map contents othercode),
                showAgentDeclaration agents,
                showPopulationDeclarations (Map.keys populations),
                showNetworkDeclarations (Map.keys networks),
                unlines (map (showAgentWire (Map.map agent populations) (Map.map context networks) attributes)
                             (Map.toList agents)),
                showModelStructure populations networks]

showAgentDeclaration :: Map Name Agent -> String
showAgentDeclaration agents = 
    let showAgent (name, (Agent attributes)) = 
            printf "%s { %s }" name (intercalate ", " . map (showAttr) $ attributes)
        showAttr (name, str) = printf "get%s %s" (capitalize name) str
    in printf "data Agent = %s deriving Show\n" 
       (intercalate " | " . map (showAgent) $ Map.toList agents)
 
showPopulationDeclarations = unlines . map showDec where
    showDec n = printf "%s = (Map.! \"%s\") . populations" n n

showNetworkDeclarations = unlines . map showDec where
    showDec n = printf "%s = (Map.! \"%s\") . networks" n n
                             
-- HELPERS 
genDecs :: [String] -> String
genDecs strs = printf "{ %s }" (intercalate "; " strs)
linearize x = case Transform.linearizeDecl x of
                 Left err -> error err
                 Right str -> str

prettify x = case Transform.prettifyDecl x of
               Left err -> error err 
               Right str -> str

-- END HELPERS
-- showAgentWire populations networks attributes agent
-- 
showAgentWire :: Map Name Name -> Map Name NetworkContext -> Map Name Attribute -> (Name, Agent) -> String
showAgentWire populations networks attributes (name, Agent fields)  = prettify $
    printf "wire%s g id = purifyRandom (helper %s) g where %s"
           name (unwords localWireNames) (genDecs localDecs)
    where fromRight (Right x) = x 
          fieldNames = map fst fields
          activeFieldNames = Map.keys agentAttributes
          localWireNames = map (printf "%sWire") activeFieldNames
          localDecs = [helper] ++ env ++ localWires

          -- the helper declaration
          helper = printf "helper %s = mkGen $ \\dt x -> do { %s ; return (Right $ (prevState x) %s, helper %s)}"
                   (unwords localWireNames) stepLocalWires updateAttributes (unwords (map (++"'") localWireNames))
          stepLocalWires = intercalate ";" (map stepLocalWire activeFieldNames)
          stepLocalWire name = printf "(Right %sNew, %sWire') <- stepWire %sWire dt x" name name name
          updateAttributes = if null activeFieldNames 
                             then "" 
                             else printf "{ %s }" (intercalate "," (map updateAttribute activeFieldNames))
          updateAttribute name = printf "get%s = %sNew" (capitalize name) name

          -- the environment
          env = map mkWire fieldNames ++ networkAttributes
          mkWire name = printf "%s = function (get%s . prevState)" name (capitalize name)
          networkAttributes = concatMap extractAttributes (Map.toList networks)
          extractAttributes :: (Name, NetworkContext) -> [String]
          extractAttributes (networkName, Symmetric {population = population , access = (_, accessName)}) = 
              if populations Map.! population == name
              then [printf "%s = networkView id view1 %s %s" accessName networkName population]
              else []
          -- TODO add cases for other network types

          -- the local wires
          agentAttributes = Map.filterWithKey (\name _ -> name `elem` fieldNames) attributes
          localWires = map localWire (Map.toList agentAttributes)
          localWire (attribute, Attribute (HaskellBlock (code))) = 
              linearize (attribute ++ "Wire = " ++ code)

   
showModelStructure :: Map Name Population -> Map Name Network -> String
showModelStructure populations networks = prettify $
    printf "modelStructure = ModelStructure { %s } where %s " (intercalate ", " components) (genDecs wires) where
        populationNames = Map.keys populations
        networkNames = Map.keys networks
        toMap op = printf "Map.fromList [%s]" . intercalate ","  . map (pair op)
        pair op name = printf "(%s, %s)" (show name) (op name)
        components = map (uncurry (printf "%s = %s")) pairs
        pairs = [("populationNames", show populationNames),
                 ("networkNames", show networkNames),
                 ("removalWires", toMap  (++"Remove") populationNames),
                 ("additionWires", toMap (++"Add") populationNames),
                 ("newAgentWires", toMap (("wire"++) . agent . (populations !)) populationNames),
                 ("networkEvolutionWires", toMap getEvolution networkNames),
                 ("networkPopulations", toMap (show . getPops . context . (networks !)) networkNames)]
        -- get the two populations (could be one) associated with the network
        getPops :: NetworkContext -> (Name, Name)
        getPops (Symmetric {population, ..}) = (population, population)
        getPops (Asymmetric {population, ..}) = (population, population)
        getPops (Bipartite {population1, population2, ..}) = (population1, population2)
        -- get the network evolution wire
        getEvolution :: String -> String
        getEvolution name = case context (networks ! name) of 
                              Symmetric {population, ..} -> printf "%sWire %s" name population
                              Asymmetric {population, ..} -> printf "%sWire %s" name population
                              Bipartite {population1, population2, ..} -> printf "%sWire %s %s" name 
                                                                          population1 population2



        wires = removalWires ++ additionWires ++ networkWires
        removalWires = map removalWire (Map.toList populations)
        additionWires = map additionWire (Map.toList populations)
        networkWires = map networkWire (Map.toList networks)
        removalWire (name, population) = 
            linearize (printf "%sRemove = %s" name (contents (removal (population))))
        additionWire (name, population) = 
            linearize (printf "%sAdd = %s" name (contents (addition (population))))
        networkWire (name, network) = 
            linearize (printf "%sWire = %s" name (contents (networkSpec network)))