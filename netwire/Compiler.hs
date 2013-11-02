{-# LANGUAGE DeriveDataTypeable #-}
-- The Frabjous Code Generator 

import Debug.Trace
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellDef)

import Data.Maybe (mapMaybe, fromMaybe, fromJust)
import Data.Either (rights)
import Data.List
import Text.Printf (printf)
import Data.Data (toConstr, Typeable, Data)
import Data.Function (on)
import qualified Language.Haskell.Exts.Parser as Haskell -- needing for working with haskell source code
import qualified Language.Haskell.Exts.Pretty as Haskell -- needed for working with haskell source code
import qualified Language.Haskell.Exts.Syntax as Haskell
import Text.Regex (subRegex, mkRegex) -- used for code generation





agentKeyword = "agent"
variableKeyword = "reactive"
populationKeyword = "population"
removalKeyword = "removal"
additionKeyword = "addition"
networkKeyword = "network"
keywords = [agentKeyword, variableKeyword, populationKeyword, removalKeyword, networkKeyword] 
   



-- abstract syntax
type Name = String
type HaskellString = String

data Program = Program {preamble :: HaskellString,
                        decs :: [Dec]} deriving Show

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


---------------------------------
---------------------------------
---------------------------------
------  UTILITIES
--------------------------------
---------------------------------
---------------------------------

decType :: Dec -> Int
decType (AgentDec _ _ ) = 0
decType (VariableDec _ _) = 1
decType (PopulationDec _ _ _ _) = 2
decType (NetworkDec _ _ _) = 3

instance Ord Dec where
    compare = compare `on` decType

type Field = (Name, HaskellString)

name :: Dec -> Name
name (AgentDec n _) = n
name (VariableDec n _) = n
name (PopulationDec n _ _ _ ) = n



instance Show Dec where
    show (AgentDec name fields) = 
        printf "data %s = %s { %s } deriving Typeable\n" 
               name 
               name 
               (intercalate ", " . map (showField) $ fields')
        where showField (name, str) = printf "_%s %s" name str
              fields' = fields ++ [("idx" ++ name, ":: Int")]-- add index field
    show (VariableDec name code) = 
        processReactiveSyntax . preprocessReactives $ printf "%sWire %s" name code
                 
                                                                                                         
    show (PopulationDec name _ _ _ ) = "Population " ++ name

-- replaces all occurences of var(t) with (arr (get var)), for any identifier var
preprocessReactives :: HaskellString -> HaskellString
preprocessReactives = clearTs . processTs where
    clearTs input = subRegex (mkRegex "\\(t\\)") input ""
    processTs input = subRegex (mkRegex "[A-z0-9_]+\\(t\\)") input "(arr (get \\0))"

                                 
-- desugars the reactive syntax "{{" and "}}"
openReactive = string "{{"
closeReactive = string "}}"
processReactiveSyntax :: HaskellString -> HaskellString
processReactiveSyntax str = 
    unlines $ map parseDec  (splitReactive str) where
        parseDec str = case (parse reactiveDec "(unknown - desugaring)" str) of 
                         Left err -> str -- leave it as is, probably a type declaration
                         Right dec -> show dec



ppOneLine = Haskell.prettyPrintWithMode oneLineMode where
    oneLineMode = Haskell.defaultMode {Haskell.layout = Haskell.PPNoLayout}
-- converts a haskell declaration to an equivalent one-line declaration 
-- by inserting braces and semicolons in accordance with the layout rules
toOneLine :: String -> Maybe String
toOneLine = (fmap ppOneLine . readDecl) where
  

readDecl:: String -> Maybe Haskell.Decl
readDecl str = case Haskell.parseDecl str of 
             Haskell.ParseOk decl -> Just decl
             _ -> Nothing


data ReactiveDec = ReactiveDec { lhs :: HaskellString,
                                 reactiveExps :: [HaskellString], 
                                 otherExps :: [HaskellString]}


instance Show ReactiveDec where
    show (ReactiveDec lhs rexps oexps) = {- fromJust . toOneLine $ -}
        if (length rexps == 0) 
        then lhs ++ " = " ++  (oexps !! 0)
        else printf "%s = proc input -> do { %s ; returnA -< (%s) }" lhs decs finalExp where 
                          finalExp =  helper 0 oexps
                          helper :: Int -> [String] -> String
                          helper n [exp] = exp 
                          helper n (exp : exps) = exp ++ (printf " __%d " n) ++ helper (n+1) exps
                          decs = intercalate ";" (zipWith genDec rexps [0..])
                          genDec :: String -> Int -> String
                          genDec rexp n = printf "__%d <- %s -< input" n rexp

-- splits a reactive variable declaration into a bunch of reactiveDecs
-- note : uses the convention that WHERE must appear on its own line,
-- and all declarations have to be lined up to it (only one layer of declarations)
splitReactive :: HaskellString -> [HaskellString]

splitReactive str = case parse fullDeclaration "(unknown - splitting)" str of 
                      Right decs -> decs
                      Left err -> error (show err)

fullDeclaration = do 
  lines <- many (notFollowedBy (many (char ' ') >> string "where ") >>  haskellLine)
  rest <- optionMaybe (try subDecs)
  return (case rest of 
            Just decs -> concat lines : decs 
            Nothing -> [concat lines])

subDecs :: GenParser Char st [String]
subDecs = do 
  (first, indent) <- firstSubDec
  others <- many (subDec indent)
  return (first : others)
  
firstSubDec :: GenParser Char st (String, Int)
firstSubDec = do 
  blanks <- many (char ' ')
  w <- string "where "
  let total = blanks ++ w
  let n = length total
  toEol <- manyTill anyChar eol
  rest <- many (try (rhsLine n))
  return (unlines ((total ++ toEol) : rest), n)

-- in a declaration of indentation n, parse a line nested even deeper
rhsLine :: Int -> GenParser Char st String
rhsLine n = do 
  blanks <- count (n + 1) (char ' ')
  toEol <- manyTill anyChar eol
  return (blanks ++ toEol)



subDec indent = do 
  blanks <- count indent (char ' ')
  toEol <- manyTill anyChar eol
  rest <- many (try (rhsLine indent))
  return (unlines ((blanks ++ toEol): rest))

                          
  

reactiveDec :: GenParser Char st ReactiveDec
reactiveDec = do 
  lhs <- many (noneOf "\n=")
  eq <- string "= "
  (others, reactives) <- rhs
  return (ReactiveDec lhs reactives (traceShow others others))


-- parse right hand side of a reactive dec into two lists - exps inside {{ }} and exps outside
rhs = do 
  pairs <- many (try rhsPart)
  last <- many anyChar
  return (let (others, reactives) = unzip pairs in (others ++ [last], reactives)) where
                 

rhsPart :: GenParser Char st (String, String)
rhsPart = do 
  other <- manyTill anyChar (try openReactive)
  reactive <- manyTill (noneOf "\n") (try closeReactive)
  return (other, reactive)

  
  

lineWithEquals :: GenParser Char st String
lineWithEquals = do 
  line <- manyTill (noneOf "\n") (try (string " = "))
  return line
  





---------------------------------
---------------------------------
---------------------------------
------  CODE GENERATOR
--------------------------------
---------------------------------
---------------------------------

main = do
  contents <- getContents
  putStrLn (process contents)

-- given the input lines of the Frabjous program, generate Haskell code
process :: String -> String
process str = case parse program "(unknown)" str of
                Left err -> show err
                Right ast -> generateCode ast

                                 
generateCode :: Program -> String
generateCode (Program preamble decs) = 
    let mkLabelsStr = printf "mkLabels [ %s ]\n" (intercalate ", " (map ("''"++) agentNames)) 
        groupedDecs = groupBy ((==) `on` decType) (sort decs)
        agentDecs = groupedDecs !! 0
        variableDecs = groupedDecs !! 1
        populationDecs = groupedDecs !! 2
        networkDecs = if (length groupedDecs >= 3) then groupedDecs !! 3 else []
        agentNames = map (name) agentDecs
    in unlines [preamble, 
                concatMap show agentDecs, 
                mkLabelsStr,  
                unlines . map (show) $ variableDecs,
                showAgentInstances agentDecs variableDecs,
                showModelDecs populationDecs,
                showInitialState populationDecs,
                showModelEvolution populationDecs networkDecs]


-- showAgentInstances prints instance declarations for every agent,
-- including the code for the agent's local change wire
-- precondition agentDecs composed of agents, variableDecs composed of variables
showAgentInstances :: [Dec] -> [Dec] -> String
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
        fieldNames :: Dec -> [Name]
        fieldNames (AgentDec _ fields) = map (fst) fields
        allVarNames = map name variableDecs

-- showModelDecs prints declarations for ModelState and ModelOutput
-- currently just contains the population; later will contain global reactive variables also
showModelDecs :: [Dec] -> String
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
showInitialState :: [Dec] -> String
showInitialState populationDecs = 
    printf "initialModelState :: ModelOutput -> ModelState\n\
           \initialModelState initialOutput =\n\
           \   ModelState %s where{ %s }\n" 
           (intercalate " " $  map (showState) populationDecs)
           whereDecs 
        where whereDecs = intercalate " ; " $ map showWires populationDecs
              showWires :: Dec -> String
              showWires (PopulationDec name agent removal addition) = 
                  let removeDec = printf "%sRemove = %s\n" name (fromMaybe "never" removal)
                      addDec = printf "%sAdd = %s\n" name (fromMaybe "never" addition)
                  in case (toOneLine removeDec, toOneLine addDec) of 
                       (Just remove, Just add) -> remove ++ " ; " ++ add
                       _ -> error "could not parse removal / addition wires"
              showState :: Dec -> String
              showState (PopulationDec name agent removal addition) = 
                   printf "(evolvePopulation (PopulationState (%s) %sRemove %sAdd))" 
                              (wiresDec :: String)
                              name
                              name
                       where wiresDec = 
                                 (printf "replicate (length (get %s initialOutput)) localChangeWire" name)
                   
-- prints the model evolution wire (handles side effects such as death and network change)                     
showModelEvolution :: [Dec] -> [Dec] -> String
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
                  modifyNetwork :: Dec -> Int -> String
                  modifyNetwork (NetworkDec (n1,l1) Nothing _) n = 
                      printf "modify %s (computeNetworkSelf %s network%d)" n1 l1 n
                  modifyNetwork (NetworkDec (n1,l1) (Just (n2,l2)) _) n = 
                      printf "modify (pairLabel (%s, %s)) (computeNetwork (%s, %s) network%d)" 
                             n1 n2 l1 l2 n    
              newPops = intercalate " " . map (++"New") $ popNames
              networkWheres = intercalate "; " $ zipWith networkWhere networkDecs [0..] where
                  networkWhere :: Dec -> Int -> String
                  networkWhere (NetworkDec _ _ code) n =
                      case toOneLine (printf "network%d = %s" n code) of 
                        Just result -> result
                        Nothing -> error "could not parse network declaration"
                                            
              
            
            
                                 


---------------------------------
---------------------------------
---------------------------------
------ PARSER
--------------------------------
---------------------------------
---------------------------------


program :: GenParser Char st Program
program = do
  preamble <- many haskellBlock
  decs <- many (skipMany space >> dec)
  return (Program (unlines preamble) decs)

-- reads a declaration and any trailing whitespace / comments
dec :: GenParser Char st Dec
dec = do
  dec <- choice . map try $ [agentDec, variableDec, populationDec, networkDec]
  return dec


haskellBlock :: GenParser Char st HaskellString
haskellBlock = do
  fstLine <- haskellLine
  lines <- many (indentedHaskellLine)
  return (unlines (fstLine : lines))


eol = char '\n'




lexer = Token.makeTokenParser haskellDef
identifier = Token.identifier lexer
braces = Token.braces lexer
commaSep1 = Token.commaSep1 lexer
symbol = Token.symbol lexer
whiteSpace = Token.whiteSpace lexer

haskellLine = do 
  notFollowedBy (choice (map symbol keywords))
  str <- many (noneOf "\n") 
  eol
  return str

indentedHaskellLine = do
  firstChar <- oneOf " \t"
  rest <- haskellLine
  return (firstChar : rest)


agentDec :: GenParser Char st Dec
agentDec = do 
  symbol agentKeyword
  id <- identifier
  fields <- braces (commaSep1 field)
  return (AgentDec id fields)

field = do
  fieldName <- identifier
  cc <- symbol "::" 
  ty <- many (noneOf ",{}\n")
  optional eol
  whiteSpace
  return (fieldName, cc ++ " " ++ ty)

variableDec :: GenParser Char st Dec
variableDec = do 
  symbol variableKeyword
  id <- identifier
  code <- haskellBlock
  return (VariableDec id code)


populationDec :: GenParser Char st Dec
populationDec = do
  symbol populationKeyword
  name <- identifier
  symbol "of"
  agent <- identifier
  r <- optionMaybe removalDec
  a <- optionMaybe additionDec
  return (PopulationDec name agent r a)

removalDec, additionDec :: GenParser Char st HaskellString
removalDec = do 
  symbol removalKeyword
  symbol "="
  code <- haskellBlock
  return code

additionDec = do
  symbol additionKeyword
  symbol "="
  code <- haskellBlock
  return code

networkDec = do 
  symbol networkKeyword
  p1 <- idPair
  p2 <- optionMaybe (symbol "with" >> idPair)
  symbol "by"
  code <- haskellBlock
  return (NetworkDec p1 p2 code)
  

idPair = do
  i1 <- identifier
  i2 <- identifier
  return (i1, i2)
  