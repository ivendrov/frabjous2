-----------------------------------------------------------------------------
-- |
-- Module      :  CodeGen
-- Copyright   :  (c) University of Saskatchewan 2013
-- 
-- Maintainer  :  ivan.vendrov@usask.ca
--
-- The Code Generator for the Frabjous programming language
-- (given a Frabjous program, generates the Haskell code for it, minus header and footer info)
--------------------------------------------------------------------------
module CodeGen (generateCode) where 



import Data.Maybe (mapMaybe, fromMaybe, fromJust)
import Data.List
import Data.Function (on)
import Text.Printf (printf)


import Syntax
import qualified Transform

-- | generate code for a Frabjous Block 
genBlock :: Block -> String
genBlock block = 
    case block of 
      (AgentDec name fields) ->
          printf "data %s = %s { %s } deriving Typeable\n" 
                 name 
                 name 
                 (intercalate ", " . map (showField) $ fields')
          where showField (name, str) = printf "_%s %s" name str
                fields' = fields ++ [("idx" ++ name, ":: Int")]-- add index field

      (VariableDec name (HaskellBlock code)) ->
          printf "%sWire %s" name code
                 
      (PopulationDec name _ _ _ ) -> "Population " ++ name
      (JustHaskell (HaskellBlock contents)) -> contents




generateCode :: Program -> String
generateCode (Program blocks) = 
    let mkLabelsStr = printf "mkLabels [ %s ]\n" (intercalate ", " (map ("''"++) agentNames)) 
        agentDecs = filter ( (==0) . decType ) blocks
        variableDecs = filter ( (==1) . decType ) blocks
        populationDecs = filter ( (==2) . decType ) blocks
        networkDecs = filter ( (==3) . decType ) blocks
        agentNames = map (name) agentDecs
    in unlines [concatMap genBlock agentDecs, 
                mkLabelsStr,  
                unlines . map (genBlock) $ variableDecs,
                showAgentInstances agentDecs variableDecs,
                showModelDecs populationDecs,
                showInitialState populationDecs,
                showModelEvolution populationDecs networkDecs]











-- showAgentInstances prints instance declarations for every agent,
-- including the code for the agent's local change wire
-- precondition agentDecs composed of agents, variableDecs composed of variables
showAgentInstances :: [Block] -> [Block] -> String
showAgentInstances agentDecs variableDecs = 
    concatMap agentInstance agentDecs where
        agentInstance agent = 
            printf "instance Agent %s where \n\
                   \    idx = idx%s\n\
                   \    localChangeWire = helper %s where\n\
                   \           helper %s =\n\
                   \              mkGen $ \\dt x -> do\n%s\
                   \                return (Right $ x {%s},\
                                   \ helper %s)\n"
                   (name agent)
                   (name agent)
                   (intercalate " " wireNames)
                   (intercalate " " wireNames)
                   (concatMap wireEvolution varNames)
                   fieldAssignments
                   (intercalate " " newWireNames) where
                       varNames = fieldNames agent `intersect` allVarNames
                       wireNames = map (++"Wire") varNames
                       wireEvolution :: String -> String
                       wireEvolution var = 
                           printf "                (Right %sNew, %sWire') <- stepWire %sWire dt x\n"
                                  var var var
                       fieldAssignments = intercalate ", " . map (showField) $ varNames
                       showField var = printf "_%s = %sNew" var var
                       newWireNames = map (++"'") wireNames
        fieldNames :: Block -> [Name]
        fieldNames (AgentDec _ fields) = map (fst) fields
        allVarNames = map name variableDecs

-- showModelDecs prints declarations for ModelState and ModelOutput
-- currently just contains the population; later will contain global reactive variables also
showModelDecs :: [Block] -> String
showModelDecs populationDecs = 
    let populationNames = map (name) populationDecs
        agentNames = map (\ (PopulationDec _ a _ _) -> a) populationDecs
        stateFields =  zipWith stateField populationNames agentNames
        outputFields = zipWith outputField populationNames agentNames
        stateField, outputField :: Name -> Name -> String
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
showInitialState :: [Block] -> String
showInitialState populationDecs = 
    printf "initialModelState :: ModelOutput -> ModelState\n\
           \initialModelState initialOutput =\n\
           \   ModelState %s where{ %s }\n" 
           (intercalate " " $  map (showState) populationDecs)
           whereDecs 
        where whereDecs = intercalate " ; " $ map showWires populationDecs
              showWires :: Block -> String
              showWires (PopulationDec name agent removal addition) = 
                  let removeDec = printf "%sRemove = %s\n" name (fromMaybe "never" (fmap contents removal))
                      addDec = printf "%sAdd = %s\n" name (fromMaybe "never" (fmap contents addition))
                  in case (Transform.linearizeDecl removeDec, Transform.linearizeDecl addDec) of 
                       (Just remove, Just add) -> remove ++ " ; " ++ add
                       _ -> error "could not parse removal / addition wires"
              showState :: Block -> String
              showState (PopulationDec name agent removal addition) = 
                   printf "(evolvePopulation (PopulationState (%s) %sRemove %sAdd))" 
                              (wiresDec :: String)
                              name
                              name
                       where wiresDec = 
                                 (printf "replicate (length (get %s initialOutput)) localChangeWire" name)
                   
-- prints the model evolution wire (handles side effects such as death and network change)                     
showModelEvolution :: [Block] -> [Block] -> String
showModelEvolution populationDecs networkDecs = 
    printf "evolveModel :: ModelState -> WireP ModelOutput ModelOutput\n\
           \evolveModel mstate =\n\
           \    mkGen $ \\dt input -> do\n%s\
           \      let\n%s\
           \      return (Right output, evolveModel (ModelState %s))\n"
           (concatMap (wireEvolution) popNames)
           bindings
           (intercalate " " (map (++ "State") popNames)) 
        where wireEvolution :: Name -> String
              wireEvolution n = 
                  printf "      (Right %sOutput, %sState) <- stepWire (get %sState mstate) dt (get %s input)\n"
                         n n n n
              bindings = unlines . map ("        "++) . lines $ (newBindings ++ output)
              popNames = map (name) populationDecs
              newBindings = concatMap (newBinding) popNames
              newBinding name = 
                  printf "%sNew = %s $ get agents %sOutput\n"
                         name 
                         (intercalate " . " . map (processDeath) . mapMaybe (otherPair) $ networkDecs)
                         name where 
                             otherPair (NetworkDec (n1,l1) (Just (n2,l2)) _) =
                                 if (name == n1) then Just (n2,l1) 
                                 else if (name == n2) then Just (n1,l2)
                                 else Nothing
                             otherPair (NetworkDec (n1,l1) Nothing _) = 
                                 if (name == n1) then Just (n1,l1)
                                 else Nothing
                             processDeath (popName, label) = 
                                 printf "processDeath %s %sNew (get removedIndices %sOutput)" 
                                        label popName popName
              output = printf "output = %s $ ModelOutput %s where{ %s }\n" updateNetworks newPops networkWheres
              updateNetworks, newPops, networkWheres :: String
              updateNetworks = intercalate " . " (zipWith modifyNetwork networkDecs [0..]) where 
                  modifyNetwork :: Block -> Int -> String
                  modifyNetwork (NetworkDec (n1,l1) Nothing _) n = 
                      printf "modify %s (computeNetworkSelf %s network%d)" n1 l1 n
                  modifyNetwork (NetworkDec (n1,l1) (Just (n2,l2)) _) n = 
                      printf "modify (pairLabel (%s, %s)) (computeNetwork (%s, %s) network%d)" 
                             n1 n2 l1 l2 n    
              newPops = intercalate " " . map (++"New") $ popNames
              networkWheres = intercalate "; " $ zipWith networkWhere networkDecs [0..] where
                  networkWhere :: Block -> Int -> String
                  networkWhere (NetworkDec _ _ (HaskellBlock code)) n =
                      case Transform.linearizeDecl (printf "network%d = %s" n code) of 
                        Just result -> result
                        Nothing -> error "could not parse network declaration"