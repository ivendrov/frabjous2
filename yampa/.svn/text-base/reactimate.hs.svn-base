-- reactimate.hs
-- 
-- a test for using reactimate


{-# LANGUAGE Arrows #-}

import FRP.Yampa
import FRP.Yampa.Event
import Warehouse


ioTest :: IO ()
ioTest = reactimate init sense actuate sf
	where
		init = putStr "init\n"
		sense b =  return (0.1, Nothing)	--ignoring the argument (for blocking IO, I don't care at this point)
		actuate b str = do { putStr str; return False}	--ignoring the argument (for whether state has changed; I'll assume it has always changed)
		sf = constant "hello\n"

ioTest2 :: IO ()
ioTest2 = reactimate init sense actuate sf
	where
		init = do {putStr "Doubler\n"; l <- getLine; return l}
		sense b = do { l <- getLine; return (0.1, Just l)} --ignore b
		actuate b str = do {putStr (str++"\n"); return (str == "quitquit")}
		sf = proc str -> do
			returnA -< str ++ str

ioTest3 :: IO ()
ioTest3 = reactimate init sense actuate sf
	where
		init = do {putStr "Warehouse\n"; input <- getLine; return input}
		sense b = do {l <- getLine; return (0.1, Just l)} --placeholder; ignore blocking (b), ignore DTime
		actuate b str = do {if (str == "") then putStr "" else putStr (str++"\n"); return False}
		sf = arr wIHandler >>> arr maybeToEvent >>> alarm


eventWrapper :: a -> Event a
eventWrapper x = Event x



wIHandler :: String -> Maybe Sensing
wIHandler str = case str of
					"do" -> Just DoorOpened
					"dc" -> Just DoorClosed
					"wo" -> Just WindowOpened
					"wc" -> Just WindowClosed
					"t"  -> Just (TempChangedTo 40)			--placeholder
					_	 -> Nothing

