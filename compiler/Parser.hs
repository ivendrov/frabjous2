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
attributeKeyword = "reactive"
populationKeyword = "population"
removalKeyword = "removal"
additionKeyword = "addition"
networkKeyword = "network"
keywords = [agentKeyword, attributeKeyword, populationKeyword, removalKeyword, networkKeyword] 

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
  many block
  getState

-- reads any number of whitespace /comments followed by a block
block :: ProgramParser ()
block = do
  whiteSpace
  dec <- choice . map try $ [agentDec, attributeDec, populationDec, networkDec, justHaskell]
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
commaSep1 = Token.commaSep1 lexer
symbol = Token.symbol lexer
whiteSpace = Token.whiteSpace lexer
parens = Token.parens lexer


agentDec = do 
  symbol agentKeyword
  name <- identifier
  fields <- braces (commaSep1 field)
  updateState (addAgent name (Agent fields))

field = do
  fieldName <- identifier
  cc <- symbol "::" 
  ty <- many (noneOf ",{}\n")
  optional eol
  whiteSpace
  return (fieldName, cc ++ " " ++ ty)


attributeDec = do
  symbol attributeKeyword
  name <- identifier
  isReactiveSyntax <- (symbol "(t)" >> return True) <|> return False
  symbol "="
  code <- haskellBlock  
  let attribute = if isReactiveSyntax
                  then Attribute (desugarReactiveSyntax code)
                  else Attribute code
  updateState (addAttribute name attribute)


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
