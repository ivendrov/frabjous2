-- waitUntil.hs
--
-- Test for handling an event

{-# LANGUAGE Arrows #-}

import FRP.Yampa
import FRP.Yampa.Utilities
import IO


smplPer = 0.01
myEvent = after 10.0 ()


