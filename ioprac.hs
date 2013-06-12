-- IO Practice

main :: IO ()
main = do
	n <- getName
	insult n

getName :: IO String
getName = do 
		putStr "Hello, there!\n"
		putStr "What is your name?\n"
		name <- getLine
		putStr ("Hello " ++ name ++ "!\n")
		return name

insult :: String -> IO ()
insult name = putStr ("You suck, "++name++"!\n")

