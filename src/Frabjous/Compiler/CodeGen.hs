{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Frabjous.Compiler.CodeGen
-- Copyright   :  (c) University of Saskatchewan 2013
-- 
-- Maintainer  :  ivan.vendrov@usask.ca
--
-- The Code Generator for the Frabjous programming language
-- (given a Frabjous model, generates the Haskell code for it, minus header and footer info)
--------------------------------------------------------------------------
module Frabjous.Compiler.CodeGen (generateCode) where 



import Data.Maybe (isJust)
import Data.List
import Text.Printf (printf)
import Data.Char (toUpper)
import Data.Map (Map, (!))
import qualified Data.Map as Map

import Frabjous.Compiler.Syntax
import qualified Frabjous.Compiler.Transform as Transform

-- capitalizes the first letter of a given attribute
capitalize, toAccessor, toInit :: String -> String
capitalize [] = []
capitalize (h : t) = toUpper h : t

-- converts a field name to an accessor to be used inside of reactive declaration
-- (currently converts "income" to "getIncome", for example
toAccessor str = "get" ++ capitalize str

-- converts a field name to the binding of its initial value
toInit str = "init" ++ capitalize str


generateCode :: Program -> String
generateCode (Program agents populations networks statistics initial othercode) = 
    if (Map.null initial) then error "no initial state specified!"
    else
        unlines [unlines (map contents othercode),
                showAgentDeclaration agents,
                showPopulationDeclarations (Map.keys populations),
                showNetworkDeclarations (Map.keys networks),
                unlines (map (showAgentWire (Map.map agent populations) (Map.map context networks))
                             (Map.toList agents)),
                showModelStructure populations networks,
                showStats statistics,
                showInitialState populations networks initial]

showAgentDeclaration :: Map Name Agent -> String
showAgentDeclaration agents = 
    let showAgent (name, (Agent attributes)) = 
            printf "%s { %s }" name (intercalate ", " . map (showAttr) $ attributes)
        showAttr (Attribute {name, annotation, code =_}) = printf "%s %s" name annotation
    in printf "data Agent = %s deriving Show\n" 
       (intercalate " | " . map (showAgent) $ Map.toList agents)
       ++ showAttributeAccessors (nub . concatMap (map name . attributes) $ Map.elems agents)

showAttributeAccessors :: [Name] -> String
showAttributeAccessors = unlines . map showAccessor where
    showAccessor name = printf "%s = %s" (toAccessor name) name
 
showPopulationDeclarations, showNetworkDeclarations :: [Name] -> String 
showPopulationDeclarations = unlines . map showDec where
    showDec n = printf "%s = (Map.! \"%s\") . populations" n n

showNetworkDeclarations = unlines . map showDec where
    showDec n = printf "%s = (Map.! \"%s\") . networks" n n
                             
-- HELPERS 
genDecs :: [String] -> String
genDecs strs = printf "{ %s }" (intercalate "; " strs)

linearize, prettify, desugar :: String -> String
linearize x = case Transform.linearizeDecl x of
                 Left err -> error err
                 Right str -> str

prettify x = case Transform.prettifyDecl x of
               Left err -> error err 
               Right str -> str

desugar x = case Transform.desugarDecl x of 
              Left err -> error err
              Right str -> str

toMap :: (String -> String) -> [String] -> String
toMap op = printf "Map.fromList [%s]" . intercalate ","  . map (pair op) where
                pair op name = printf "(%s, %s)" (show name) (op name)

-- END HELPERS
-- showAgentWire populations networks attributes agent
-- 
showAgentWire :: Map Name Name -> Map Name NetworkContext -> (Name, Agent) -> String
showAgentWire populations networks (agentName, Agent attributes)  = prettify $
    printf "wire%s g id initAgent = purifyRandom (helper %s) g where %s"
           agentName (unwords localWireNames) (genDecs localDecs)
    where fieldNames = map name attributes
          activeFieldNames = map name activeAttributes
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
          updateAttribute name = printf "%s = %sNew" name name

          -- the environment
          env = map mkWire fieldNames ++ networkAttributes ++ map initAttribute fieldNames
          mkWire name = printf "%s = function (get%s . prevState)" name (capitalize name)
          networkAttributes = concatMap extractAttributes (Map.toList networks)
          extractAttributes :: (Name, NetworkContext) -> [String]
          extractAttributes (networkName, Symmetric {population = population , access = (_, accessName)}) = 
              if populations Map.! population == agentName
              then [printf "%s = arr (fromMany) . networkView id view1 %s %s" accessName networkName population]
              else []
          -- TODO add cases for other network types
          initAttribute name = printf "%s = %s initAgent" (toInit name) (toAccessor name)

          -- the local wires
          activeAttributes = filter (isJust . code) attributes
          localWires = map localWire activeAttributes
          localWire (Attribute {name = name, code = Just (HaskellBlock (rhs)), ..}) = 
              desugar (name ++ "Wire " ++ rhs)

   
showModelStructure :: Map Name Population -> Map Name Network -> String
showModelStructure populations networks = prettify $
    printf "modelStructure = ModelStructure { %s } where %s " (intercalate ", " components) (genDecs wires) where
        populationNames = Map.keys populations
        networkNames = Map.keys networks
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

showStats :: Map Name HaskellBlock -> String
showStats statistics = unlines (mainDec : statDecs) where
    mainDec = "statistics = " ++ toMap ("arr show . "++) (Map.keys statistics)
    statDecs = zipWith (printf "%s = %s") (Map.keys statistics) (map contents (Map.elems statistics))

showInitialState :: Map Name Population  -> Map Name Network -> Map Name HaskellBlock -> String
showInitialState populations networks initials = prettify $
    printf "initialState = do {startingPops <- startingPopulations; \
                               \return $ InitialState startingPops startingNetworks} where %s"
    (genDecs initialDecs) where
        initialDecs = startingPops : startingNetworks : decs                         
        startingPops = "startingPopulations = Traversable.sequence $ "
                       ++ toMap (++"Initial") (Map.keys populations)
        startingNetworks = "startingNetworks = " ++ toMap (++"Initial") (Map.keys networks)
        decs =  map (uncurry (printf "%sInitial = %s")) (Map.toList (Map.map contents initials))

                                                    

    
    