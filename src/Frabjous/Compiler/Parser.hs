-----------------------------------------------------------------------------
-- |
-- Module      :  Frabjous.Compiler.Parser
-- Copyright   :  (c) University of Saskatchewan 2013
-- License     :  BSD-style 
-- 
-- Maintainer  :  ivan.vendrov@usask.ca
-- Stability   :  experimental
-- Portability :  portable
--
-- The Parser for the Frabjous programming language
--------------------------------------------------------------------------------------------------------------
module Frabjous.Compiler.Parser (parseProgram) where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellDef)
import Control.Applicative ((<*))

import Frabjous.Compiler.Syntax

-- LEXICAL ISSUES
agentKeyword = "agent"
populationKeyword = "population"
removalKeyword = "removal"
additionKeyword = "addition"
networkKeyword = "network"
statisticKeyword = "statistic"
initialKeyword = "initialState"
keywords = [agentKeyword, populationKeyword, 
            removalKeyword, networkKeyword, statisticKeyword,
           initialKeyword] 

eol = try (string "\n\r") <|> string "\n" <?> "expected end of line"
line = many (noneOf "\n\r") <* eol
indentedLine = do
  firstChar <- oneOf " \t"
  rest <- line
  return (firstChar : rest)




-- | parses a full Frabjous program from the given string
parseProgram :: String -> Either ParseError Program
parseProgram = runParser program emptyProgram "unknown"

type ProgramParser = GenParser Char Program
program :: ProgramParser Program
program = do 
  many (try block)
  getState

-- reads any number of whitespace /comments followed by a block
block :: ProgramParser ()
block = do
  whiteSpace
  dec <- choice . map try $ [agentDec, populationDec, networkDec, statisticDec, initialState,
                             justHaskell]
  return dec


justHaskell :: ProgramParser ()
justHaskell = do
  block <- haskellBlock
  updateState (addCode block)

haskellBlock :: ProgramParser HaskellBlock
haskellBlock = do
  notFollowedBy (choice (map symbol keywords))
  fstLine <- line
  lines <- many (indentedLine)
  return (HaskellBlock $ unlines (fstLine : lines))







lexer = Token.makeTokenParser haskellDef
identifier = Token.identifier lexer
braces = Token.braces lexer
semiSep1 = Token.semiSep1 lexer
symbol = Token.symbol lexer
whiteSpace = Token.whiteSpace lexer
parens = Token.parens lexer
semi = Token.semi lexer


agentDec = do 
  symbol agentKeyword
  name <- identifier
  attributes <- braces (semiSep1 attributeDec)
  updateState (addAgent name (Agent attributes))

attributeDec = do
  fieldName <- identifier
  reactiveSyntax <- symbol "t" <|> return ""
  cc <- symbol "::" 
  ty <- many (noneOf ";{}\n=")
  optional eol
  whiteSpace -- TODO be able to read multiple empty lines
  code <- many (noneOf ";{}")
  whiteSpace -- TODO same here
  return (Attribute{ name = fieldName, 
                     annotation = cc ++ " " ++ ty,
                     code = if (null code) then Nothing
                             else Just $ HaskellBlock $ reactiveSyntax ++ code}
                     )


populationDec = do
  symbol populationKeyword
  name <- identifier
  symbol "of"
  agent <- identifier
  r <- try removalDec <|> return (HaskellBlock "never")
  a <- try additionDec <|> return (HaskellBlock "never")
  updateState (addPopulation name (Population agent r a))

removalDec, additionDec :: ProgramParser HaskellBlock
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
  name <- identifier
  symbol "between"
  context <- symmetricContext -- TODO give options for asymmetric and bipartite networks as well
  symbol "="
  code <- haskellBlock
  updateState (addNetwork name (Network {context = context, networkSpec = code}))
         where symmetricContext = do
                 a <- networkAccess
                 symbol "in"
                 p <- identifier
                 return (Symmetric { population = p, access = a})
  
networkAccess :: GenParser Char st NetworkAccess
networkAccess = do
  Just link <- fmap readLink identifier
  name <- identifier
  return (link, name)

statisticDec = do
  symbol statisticKeyword
  name <- identifier
  symbol "=" 
  code <- haskellBlock
  updateState (addStatistic name code)

initialState = do
  symbol initialKeyword
  bindings <- braces (semiSep1 binding)
  updateState (addInitial bindings)
  
binding = do 
  whiteSpace
  name <- identifier
  symbol "="
  code <- many (noneOf ";{}")
  return (name, HaskellBlock code)