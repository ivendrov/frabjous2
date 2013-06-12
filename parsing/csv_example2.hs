--from Real World Haskell
--pg 387
--parses CSV files (lines of records, fields separated by commas)

import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
--eol = char '\n'
eol =	try (string "\n\r")
	<|> try (string "\r\n")
	--these are only one character long so no need to try
	<|> string "\n"
	<|> string "\r"
--	<|> fail "Couldn't find EOL"
	<?> "end of line"


parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

