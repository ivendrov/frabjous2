--from Real World Haskell
--pg 391, 392
--parses CSV files (lines of records, fields separated by commas)

import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell =
	do 	char '"'
		content <- many quotedChar
		char '"' <?> "quote at end of cell"
		return content

quotedChar = 
	noneOf "\""
	<|> try (string "\"\"" >> return '"')

eol =	try (string "\n\r")
	<|> try (string "\r\n")
	--these are only one character long so no need to try
	<|> string "\n"
	<|> string "\r"
--	<|> fail "Couldn't find EOL"
	<?> "end of line"


parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main =
	do 	c <- getContents
		case parse csvFile "(stdin)" c of
				Left e  -> do putStrLn "Error parsing input:"
				Right r -> mapM_ print r
