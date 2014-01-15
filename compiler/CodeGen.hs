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

import qualified Syntax
import Analyzer
import qualified Transform


generateCode :: FrabjousModel -> String
generateCode (FrabjousModel agents attributes populations networks othercode) = 
    let mkLabelsStr = printf "mkLabels [ %s ]\n" (intercalate ", " (map ("''"++) agentNames)) 
        agentNames = map (name) agents
    in unlines [othercode,
                concatMap showAgentDeclaration agents, 
                mkLabelsStr,  
                unlines . map showAttribute $ attributes,
                concatMap showAgentInstance agents,
                showModelDecs populations,
                showInitialState populations,
                showModelEvolution populations networks]



showAgentDeclaration :: Agent -> String
showAgentDeclaration (Agent name attributes _) = 
    printf "data %s = %s { %s } deriving Typeable\n" 
                 name 
                 name 
                 (intercalate ", " . map (showAttr) $ attributes')
          where showAttr (name, str) = printf "_%s %s" name str
                attributes' = attributes ++ [("idx" ++ name, ":: Int")]-- add index attribute
    
showAttribute :: Attribute -> String
showAttribute (Attribute name (Syntax.HaskellBlock code)) = printf "%sWire %s" name code


    



-- showAgentInstance prints the instance declarations for a given agent,
-- including the code for the agent's local change wire
-- (note that the actual definitions of attributes are not YET done here - TODO?)
showAgentInstance :: Agent -> String
showAgentInstance (Agent name _ attributes) = 
            printf "instance Agent %s where \n\
                   \    idx = idx%s\n\
                   \    localChangeWire = helper %s where\n\
                   \           helper %s =\n\
                   \              mkGen $ \\dt x -> do\n%s\
                   \                return (Right $ x {%s},\
                                   \ helper %s)\n"
                   name 
                   name
                   (intercalate " " wireNames)
                   (intercalate " " wireNames)
                   (concatMap wireEvolution varNames)
                   fieldAssignments
                   (intercalate " " newWireNames) where
                       varNames = map varName attributes
                       wireNames = map (++"Wire") varNames
                       wireEvolution :: String -> String
                       wireEvolution var = 
                           printf "                (Right %sNew, %sWire') <- stepWire %sWire dt x\n"
                                  var var var
                       fieldAssignments = intercalate ", " . map (showField) $ varNames
                       showField var = printf "_%s = %sNew" var var
                       newWireNames = map (++"'") wireNames

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