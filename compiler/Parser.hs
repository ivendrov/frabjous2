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
module Parser (parseProgram, parseDec) where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellDef)
import Control.Applicative ((<*), (<$>))
import Data.Maybe (maybe)
import Data.List (intercalate)
import Text.Printf (printf)
import Text.Regex (subRegex, mkRegex) -- used for reactive syntax

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
program = Program <$> many block

-- reads any number of whitespace /comments followed by a Block
block :: GenParser Char st Block
block = do
  whiteSpace
  dec <- choice . map try $ [agentDec, variableDec, populationDec, networkDec, JustHaskell <$> haskellBlock]
  return dec


haskellBlock :: GenParser Char st HaskellBlock
haskellBlock = do
  notFollowedBy (choice (map symbol keywords))
  fstLine <- line
  lines <- many (indentedLine)
  return (HaskellBlock (unlines (fstLine : lines)))







lexer = Token.makeTokenParser haskellDef
identifier = Token.identifier lexer
braces = Token.braces lexer
commaSep1 = Token.commaSep1 lexer
symbol = Token.symbol lexer
whiteSpace = Token.whiteSpace lexer
parens = Token.parens lexer






agentDec :: GenParser Char st Block
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

variableDec :: GenParser Char st Block
variableDec = do 
  symbol variableKeyword
  id <- identifier
  isReactiveSyntax <- (symbol "(t)" >> return True) <|> return False
  symbol "="
  code <- haskellBlock  
  if (isReactiveSyntax) 
  then return (VariableDec id (desugarReactiveSyntax code))
  else return (VariableDec id code)


populationDec :: GenParser Char st Block
populationDec = do
  symbol populationKeyword
  name <- identifier
  symbol "of"
  agent <- identifier
  r <- try removalDec <|> return (HaskellBlock "never")
  a <- try additionDec <|> return (HaskellBlock "never")
  return (PopulationDec name agent r a)

removalDec, additionDec :: GenParser Char st HaskellBlock
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


-- | desugars the rhs of a variable declaration using the (t) syntax
desugarReactiveSyntax :: HaskellBlock -> HaskellBlock
desugarReactiveSyntax (HaskellBlock contents) = HaskellBlock . show . parseDec $ contents


-- invariant : if you interleave otherExps with reactiveExps, you get back the original string
data ReactiveDec = ReactiveDec { reactiveExps :: [String], 
                                 otherExps :: [String]}

instance Show ReactiveDec where
    show (ReactiveDec rexps oexps) = {- fromJust . toOneLine $ -}
        if (length rexps == 0) 
        then (oexps !! 0)
        else printf "proc input -> do { %s ; returnA -< (%s) }" decs finalExp where 
                          finalExp =  helper 0 oexps
                          helper :: Int -> [String] -> String
                          helper n [exp] = exp 
                          helper n (exp : exps) = exp ++ (printf " __%d " n) ++ helper (n+1) exps
                          decs = intercalate ";" (zipWith genDec rexps [0..])
                          genDec :: String -> Int -> String
                          genDec rexp n = printf "__%d <- %s -< input" n rexp

parseDec :: String -> ReactiveDec
parseDec str = case parse reactiveDec "incorrect (t) syntax " str  of 
                 Right dec -> dec
                 Left err -> error (show err)

reactiveDec :: GenParser Char st ReactiveDec
reactiveDec = do
  first <- manyTill anyChar (lookAhead . try $ reactiveExp) -- read string before first (t)
  others <- many pairReactive -- read pairs of "reactive" and "other"
  let (reactives, otherExps) = unzip others
  return (ReactiveDec reactives (first : otherExps))

pairReactive :: GenParser Char st (String, String)
pairReactive = do
  reactive <- reactiveExp
  other <- manyTill anyChar ((lookAhead . try $ reactiveExp) <|> (eof >> return ""))
  return (reactive, other)
  

reactiveExp = do
  exp <- identifier <|> balancedParentheses
  symbol "(t)"
  return (printf "(%s)" exp)
  
balancedParentheses ::  GenParser Char st String 
balancedParentheses = do 
  exp <- parens (many (choice [try balancedParentheses, many1 (noneOf "()")]))
  return (printf "(%s)" (concat exp))
