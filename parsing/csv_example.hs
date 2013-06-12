--from Real World Haskell
--pg 383/384
--parses CSV files (lines of records, fields separated by commas)

import Text.ParserCombinators.Parsec

--contains 0 or more lines, each of which is terminated by the eol character
csvFile :: GenParser Char st [[String]]
csvFile = do
		result <- many line
		eof
		return result

--each line contains one or more cells (comma separated)
line :: GenParser Char st [String]
line = do
	result <- cells
	eol
	return result

--build up a list of cells
cells :: GenParser Char st [String]
cells = do
	first <- cellContent
	next <- remainingCells
	return (first : next)


--cell either ends with a comma, indicating one or more cells follow, or doesn't if it's the last
remainingCells :: GenParser Char st [String]
remainingCells = 
	(char ',' >> cells)	--found comma? more cells coming
	<|> (return [])		--no comma? return [], no more cells


--each cell has 0 or more characters, which must not be a comma or EOL
cellContent :: GenParser Char st String
cellContent = 
	many (noneOf ",\n")


--end of line character is "\n"
eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input


