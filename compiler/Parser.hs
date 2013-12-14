-----------------------------------------------------------------------------
-- |
-- Module      :  Parser
-- Copyright   :  (c) University of Saskatchewan 2013
-- License     :  BSD-style 
-- 
-- Maintainer  :  ivan.vendrov@usask.ca
-- Stability   :  experimental
-- Portability :  portable
--
-- The Parser for the Frabjous programming language
--------------------------------------------------------------------------------------------------------------
module Parser (parseProgram) where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellDef)
import Control.Applicative ((<*))

import Syntax

-- LEXICAL ISSUES
agentKeyword = "agent"
variableKeyword = "reactive"
populationKeyword = "population"
removalKeyword = "removal"
additionKeyword = "addition"
networkKeyword = "network"
keywords = [agentKeyword, variableKeyword, populationKeyword, removalKeyword, networkKeyword] 

eol = try (string "\n\r") <|> string "\n" <?> "expected end of line"
line = many (noneOf "\n\r") <* eol
indentedLine = do
  firstChar <- oneOf " \t"
  rest <- line
  return (firstChar : rest)




-- | parses a full Frabjous program from the given string
parseProgram :: String -> Either ParseError Program
parseProgram = parse program "unknown"

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
  notFollowedBy (choice (map symbol keywords))
  fstLine <- line
  lines <- many (indentedLine)
  return (unlines (fstLine : lines))







lexer = Token.makeTokenParser haskellDef
identifier = Token.identifier lexer
braces = Token.braces lexer
commaSep1 = Token.commaSep1 lexer
symbol = Token.symbol lexer
whiteSpace = Token.whiteSpace lexer






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