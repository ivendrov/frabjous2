-- Yampa test
 
{-# LANGUAGE Arrows #-}

import FRP.Yampa
import FRP.Yampa.Utilities
import IO


smplPer = 0.01

--eventList:: SF a (Event ())
--eventList = afterEach [ (1.0, ()), (2.0, ()), (50.0, ()) ]


main :: IO ()
main = putStr ((show getTwenty) ++ "\n")

getTwenty :: [Int]
getTwenty = take 20 (map snd tios)
	where
		tios = embed (localTime &&& ((constant 3) >>> incr)) (deltaEncode smplPer (repeat()))


incr :: SF Int Int
incr = proc a -> do
		returnA -< (a+1)

--main = putStr (show (fromIntegral (round (localTime * 100)) / 100))




{-
mainLoop :: SF Int Int
mainLoop = proc a -> do
		b <- incr -< a
		putStr a
-}
{-
three = constant 3


printOut :: Show a => SF a a
printOut = proc x -> do
		putStr (show x)
		returnA <- x

-}



{-
mainLoop :: IO ()
mainLoop = reactimate init (sense True) (actuate True "foo") sf
	where
		init = getName
		sense True
		





getName :: IO String
getName = do 
		putStr "Hello, there!\n"
		putStr "What is your name?\n"
		name <- getLine
		putStr ("Hello " ++ name ++ "!\n")
		return name
-}
