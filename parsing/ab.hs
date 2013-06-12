--ab.hs
--a program that accepts a^ib^i as a language.

import Text.ParserCombinators.Parsec

lang =	nonEmptyLang
		<|> emptyLang
		<|> fail "not in grammar (a^i)(b^i)"

nonEmptyLang = ((string "a") >> lang >> (string "b"))
emptyLang = string ""


runLang input = parse (lang >> eof) "(unknown)" input
		
	
