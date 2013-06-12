--expr.hs
-- parses a mathematical expression (no subtraction or multiplication yet)
-- returns a list of tuples (operations)

import Text.ParserCombinators.Parsec
import Char

data Op = Plus | Mult
	deriving(Show)

data ParseTree = Empty | Val Int | Tree Op ParseTree ParseTree
	deriving(Show)

expr = try complexExpr <|> empty
empty = do { string ""; return Empty}
complexExpr = try compoundExpr <|> term
compoundExpr = do
		t <- term
		char '+'
		e <- complexExpr
		return (Tree Plus t e)
term = try complexTerm <|> factor
complexTerm = do
	f <- factor
	char '*'
	t <- term
	return (Tree Mult f t)
factor = nestedExpr <|> myDigit
nestedExpr = do
		char '('
		e <- expr
		char ')'
		return e
myDigit = do
	--d <- many (try digit)
	d <- digit
	return (Val (digitToInt d))

run :: String -> Either ParseError ParseTree
run input = parse (expr)  "(unknown)" input





calc :: ParseTree -> Int
calc Empty = 0
calc (Val x) = x
calc (Tree Plus x y) = calc x + calc y
calc (Tree Mult x y) = calc x * calc y


handleCalc :: Either ParseError ParseTree -> Int
handleCalc (Left _)		=  -1
handleCalc (Right tree) = calc tree



calculate :: String -> Int
calculate input = handleCalc (run input)

