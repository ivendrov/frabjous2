-- testing out TH

{-# LANGUAGE TemplateHaskell #-}

module Bar where

import Foo

cross2 = $(mkCross 2)
cross3 = $(mkCross 3)