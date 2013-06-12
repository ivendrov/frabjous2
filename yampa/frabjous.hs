-- frabjous.hs
--
-- top level functions for the Frabjous system
--
-- Oliver Schneider
-- University of Saskatchewan

{-# LANGUAGE Arrows, MultiParamTypeClasses #-}

--TODO: declare functions I want to hide (ie Prims)
module Frabjous where

import FRP.Yampa
import Random (RandomGen)

--------------------------------------
--		ISOMORPHIC TYPE CLASS		--
--------------------------------------

{-
class (StateSF b) => State a b where
	repToFn :: a -> b
	fnToRep :: b -> a
-}
--type StateSF = Int {- TODO -}

--MsgT means Target, MsgU means universal
--data Message = (State s, Prop p) => MsgT s p | MsgU p


-- ?
--class Prop a where
	{- TODO? -}

-- ?
-- class/data MessageHandler




--------------------------------------
--		TRANSITION FUNCTIONS		--
--------------------------------------


-- D is Dynamic - timeout time and rate can vary
-- F is Filter - Event thrown can vary

-- TIMEOUT
-- Inefficient, maybe messy to declare in terms of timeoutDF

timeout :: Time -> a -> SF b (Event a)
timeout t a = (constant t &&& constant a) >>> timeoutDF

timeoutD :: a -> SF Time (Event a)
timeoutD a = (identity &&& constant a) >>> timeoutDF

timeoutF :: Time -> SF a (Event a)
timeoutF t = (constant t &&& identity) >>> timeoutDF

timeoutDF :: SF (Time, a) (Event a)
timeoutDF = proc (t, a) -> do
		e <- tPrim 0 -< t
		returnA -< e `tag` a

--timeout primitive 
tPrim t = switch (base &&& changePrim t) tPrim
	where
		base = now () >>> delayEvent t --simple delay


-- RATE

rate :: RandomGen g => g -> Double -> a -> SF b (Event a)
rate g d a = constant noEvent --stub

rateD :: RandomGen g => g -> a -> SF Double (Event a)
rateD g a = constant noEvent --stub

rateF :: RandomGen g => g -> Double -> SF a (Event a)
rateF g d = constant noEvent --stub

rateDF :: RandomGen g => g -> SF (Time, a) (Event a)
rateDF g = proc (rt, a) -> do
		let (r, _) = randomR (0,1) g
		let t = - (log (1 - r)) / rt
		e <- 
		returnA -< noEvent --stub

rPrim g rt = switch (base &&& next) (rPrim g')
	where
		base = timeoutF t
		(r,g') = randomR (0,1) g
		t = - (log (1 - r)) / rt
		next = changePrim rt
		



----------------------------------
--		DECLARATION FUNCTIONS	--
----------------------------------


state output transitions = dSwitch ( (constant output) &&& transitions) id


---UTILS---
changePrim a = arr dup >>> first (arr (/= a) >>> iEdge False) >>> arr (uncurry tag)

{- TODO?
gaussRate
poissonRate
-}


