-- DiceRoller.hs
-- Oliver Schneider
--
-- an application for rolling dice


import OliDice
import Char
import IO

main :: IO ()
main = do
		putStrLn "======================================="
		putStrLn "=== Welcome to Oliver's Dice Roller ==="
		putStrLn "======================================="
		putStrLn ""
		mainloop
		
mainloop :: IO ()
mainloop = do
		putStr "\n\nNumber of dice: "
		hFlush stdout
		n <- getLine
		putStr "Size of Dice: "
		hFlush stdout
		s <- getLine
		r <- (stringToInt n) `d` (stringToInt s) 
		putStr ("Total is " ++ (intToString r))
		hFlush stdout
		mainloop


stringToInt :: String -> Int
stringToInt = foldl (\xs -> \x -> ((digitToInt x)) + 10 * xs) 0

intToString :: Int -> String
intToString 0 = []
intToString n = (intToString q)++[intToDigit r]
		where
			q = n `div` 10
			r = n `mod` 10