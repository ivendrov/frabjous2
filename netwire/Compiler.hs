{-# LANGUAGE DeriveDataTypeable #-}
-- The Frabjous Code Generator 

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellDef)

import Data.Maybe (mapMaybe)
import Data.List
import Text.Printf (printf)
import Data.Data (toConstr, Typeable, Data)
import Data.Function (on)



-- abstract syntax
type Name = String
type HaskellString = String

data Program = Program {preamble :: HaskellString,
                        decs :: [Dec]} deriving Show

data Dec = AgentDec { agentName :: Name,
                      fields :: [Field]}
         | VariableDec { varName :: Name,
                         code :: HaskellString}
           deriving (Eq, Ord, Typeable, Data)

name :: Dec -> Name
name (AgentDec n _) = n
name (VariableDec n _) = n



instance Show Dec where
    show (AgentDec name fields) = 
        printf "data %s = %s { %s } deriving Typeable\n" 
               name 
               name 
               (intercalate ", " . map (showField) $ fields')
        where showField (name, str) = printf "_%s %s" name str
              fields' = fields ++ [("idx" ++ name, ":: Int")]-- add index field
    show (VariableDec name code) = 
        printf "%sWire %s" name code

type Field = (Name, HaskellString)


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
        groupedDecs = groupBy ((==) `on` toConstr) (sort decs)
        agentDecs = groupedDecs !! 0
        variableDecs = groupedDecs !! 1
        agentNames = map (name) agentDecs
    in unlines [preamble, 
                concatMap show agentDecs, 
                mkLabelsStr,  
                concatMap show variableDecs,
                showAgentInstances agentDecs variableDecs]


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

        
                                      






program :: GenParser Char st Program
program = do
  preamble <- haskellBlock
  decs <- many dec
  return (Program preamble decs)

-- reads a declaration and any trailing whitespace / comments
dec :: GenParser Char st Dec
dec = do
  dec <- choice [agentDec, variableDec]
  many emptyLine
  whiteSpace
  return dec


haskellBlock :: GenParser Char st HaskellString
haskellBlock = do
  lines <- many (haskellLine)
  return (concat lines)


eol = char '\n'

emptyLine = do
  whiteSpace
  eol
  return ()



lexer = Token.makeTokenParser haskellDef
identifier = Token.identifier lexer
braces = Token.braces lexer
commaSep1 = Token.commaSep1 lexer
symbol = Token.symbol lexer
whiteSpace = Token.whiteSpace lexer

haskellLine = do 
  notFollowedBy (symbol "agent" <|> symbol "reactive")
  str <- many (noneOf "\n") 
  nl <- eol
  return (str ++ [nl])


agentDec :: GenParser Char st Dec
agentDec = do 
  symbol "agent"
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
  symbol "reactive"
  id <- identifier
  code <- haskellBlock
  return (VariableDec id code)

  
  