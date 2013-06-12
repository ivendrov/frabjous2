-- timescale.hs
--
-- defines time units and unit rates
--
-- Oliver Schneider
-- University of Saskatchewan

{-# LANGUAGE Arrows #-}

module TimeScale where

import FRP.Yampa
import Frabjous

type ScaledTime	= Time
type Seconds	= ScaledTime
type Minutes	= ScaledTime
type Hours		= ScaledTime
type Days		= ScaledTime
type Years		= ScaledTime

--typeclass?

--convert :: ScaledTime -> ScaledTime


--scaledRate :: ScaledTime x ->