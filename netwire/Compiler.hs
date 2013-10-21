-- The Frabjous Code Generator 

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellDef)

import Data.Maybe (mapMaybe)
import Data.List



-- abstract syntax
type Name = String
type HaskellString = String

data Program = Program {preamble :: HaskellString,
                        decs :: [Dec]} deriving Show

data Dec = AgentDec { agentName :: Name,
                      fields :: [Field]}
         | VariableDec { varName :: Name,
                         code :: HaskellString}
           deriving (Eq, Ord)

instance Show Dec where
    show (AgentDec name fields) = 
        ("data " ++ name ++ " = " ++ name ++ " {" ++
        (intercalate ", " . map (showField) $ fields') ++
        "} deriving Typeable\n")
        where showField (name, str) = "_" ++ name ++ " " ++ str
              fields' = fields ++ [("idx" ++ name, ":: Int")]-- add index field

type Field = (Name, HaskellString)


                                 
generateCode :: Program -> String
generateCode (Program preamble decs) = 
    let sortedDecs = sort (decs)
        names = mapMaybe agent decs where
            agent (AgentDec name _) = Just name
            agent _ = Nothing
        mkLabelsStr = "mkLabels [" ++ intercalate ", " (map showName names) ++ "]\n" where
            showName = ("''" ++)
        decsStr = concat (map show sortedDecs)
    in preamble ++  decsStr ++ mkLabelsStr
        
                                      

main = do
  contents <- getContents
  putStrLn (process contents)


-- given the input lines of the Frabjous program, generate Haskell code
process :: String -> String
process str = case parse program "(unknown)" str of
                Left err -> show err
                Right ast -> generateCode ast

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


variableDec = do 
  symbol "reactive"
  id <- identifier
  return (VariableDec id [])

  
  