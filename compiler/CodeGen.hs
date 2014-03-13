{-# LANGUAGE NamedFieldPuns #-}
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
                 ("networkEvolutionWires", toMap (++"Wire") networkNames),
                 ("networkPopulations", toMap (show . getPops . context . (networks !)) networkNames)]
        getPops :: NetworkContext -> (Name, Name)
        getPops (Symmetric {population, access}) = (population, population)
        getPops (Asymmetric {population, access1, access2}) = (population, population)
        getPops (Bipartite {population1, population2, access1, access2}) = (population1, population2)

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


{-
-- showModelDecs prints declarations for ModelState and ModelOutput
-- currently just contains the population; later will contain global reactive variables also
showModelDecs :: [Population] -> String
showModelDecs populations = 
    let populationNames = map populationName populations
        agentNames = map agent populations
        stateFields =  zipWith stateField populationNames agentNames
        outputFields = zipWith outputField populationNames agentNames
        stateField, outputField :: String -> String -> String
        stateField pop agent = printf "%sState :: WireP (Vector %s) (PopulationOutput %s)" pop agent agent
        outputField pop agent = printf "%s :: Vector %s" pop agent
    in printf "fclabels [d|\n\
              \    data ModelState = ModelState { %s }\n\
              \    data ModelOutput = ModelOutput { %s }\n\
              \          |]\n"
              (intercalate ", " stateFields)
              (intercalate ", " outputFields)

-- showInitialState prints the initial model state, using all the populations' specified
-- removal and addition dynamics
showInitialState :: [Population] -> String
showInitialState populations = 
    printf "initialModelState :: ModelOutput -> ModelState\n\
           \initialModelState initialOutput =\n\
           \   ModelState %s where{ %s }\n" 
           (intercalate " " $  map showState populations)
           whereDecs 
        where whereDecs = intercalate " ; " $ map showWires populations
              showWires :: Population -> String
              showWires (Population name agent removal addition) = 
                  let removeDec = printf "%sRemove = %s\n" name (Syntax.contents removal)
                      addDec = printf "%sAdd = %s\n" name (Syntax.contents addition)
                  in case (Transform.linearizeDecl removeDec, Transform.linearizeDecl addDec) of 
                       (Just remove, Just add) -> remove ++ " ; " ++ add
                       _ -> error "could not parse removal / addition wires"
              showState :: Population -> String
              showState (Population name agent removal addition) = 
                   printf "(evolvePopulation (PopulationState (%s) %sRemove %sAdd))" 
                              (wiresDec :: String)
                              name
                              name
                       where wiresDec = 
                                 (printf "replicate (length (get %s initialOutput)) localChangeWire" name)
                   
-- prints the model evolution wire (handles side effects such as death and network change)                     
showModelEvolution :: [Population] -> [Network] -> String
showModelEvolution populations networks = 
    printf "evolveModel :: ModelState -> WireP ModelOutput ModelOutput\n\
           \evolveModel mstate =\n\
           \  mkGen $ \\dt input -> do\n%s"  (concatMap (wireEvolution) popNames) ++
    printf "    let\n%s" bindings ++
    printf "    return (Right output, evolveModel (ModelState %s))\n" (intercalate " " (map (++ "State") popNames)) 
          
        where wireEvolution :: String -> String
              wireEvolution n = 
                  printf "    (Right %sOutput, %sState) <- stepWire (get %sState mstate) dt (get %s input)\n"
                         n n n n
              bindings = unlines . map ("      "++) . lines $ (newBindings ++ output)
              popNames = map populationName populations
              newBindings = concatMap (newBinding) popNames
              newBinding name = 
                  printf "%sNew = %s $ get agents %sOutput\n"
                         name 
                         (intercalate " . " . map (processDeath) . mapMaybe (otherPair) $ networks)
                         name where 
                             otherPair (Network (n1,l1) (Just (n2,l2)) _) =
                                 if (name == n1) then Just (n2,l1) 
                                 else if (name == n2) then Just (n1,l2)
                                 else Nothing
                             otherPair (Network (n1,l1) Nothing _) = 
                                 if (name == n1) then Just (n1,l1)
                                 else Nothing
                             processDeath (popName, label) = 
                                 printf "processDeath %s %sNew (get removedIndices %sOutput)" 
                                        label popName popName
              output = printf "output = %s $ ModelOutput %s where{ %s }\n" updateNetworks newPops networkWheres
              updateNetworks, newPops, networkWheres :: String
              updateNetworks = intercalate " . " (zipWith modifyNetwork networks [0..]) where 
                  modifyNetwork :: Network -> Int -> String
                  modifyNetwork (Network (n1,l1) Nothing _) n = 
                      printf "modify %s (computeNetworkSelf %s network%d)" n1 l1 n
                  modifyNetwork (Network (n1,l1) (Just (n2,l2)) _) n = 
                      printf "modify (pairLabel (%s, %s)) (computeNetwork (%s, %s) network%d)" 
                             n1 n2 l1 l2 n    
              newPops = intercalate " " . map (++"New") $ popNames
              networkWheres = intercalate "; " $ zipWith networkWhere networks [0..] where
                  networkWhere :: Network -> Int -> String
                  networkWhere (Network _ _ (Syntax.HaskellBlock code)) n =
                      case Transform.linearizeDecl (printf "network%d = %s" n code) of 
                        Just result -> result
                        Nothing -> error "could not parse network declaration"
-}