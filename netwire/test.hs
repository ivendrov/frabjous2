{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Main where

import Control.Monad hiding (when)
import Control.Monad.Identity (Identity)
import Control.Arrow
import Control.Wire hiding (avg)
import System.Random (mkStdGen)
import Prelude hiding ((.), id)
import Text.Printf


control whenInhibited whenProduced wire = loop wire clockSession where
    loop w' session' = do
      (mx, w, session) <- stepSessionP w' session' ()
      case mx of 
        Left ex -> whenInhibited ex
        Right x -> do whenProduced x
                      loop w session

average :: [Double] -> Double
average l = (sum l) / fromIntegral (length l)

baseIncomes :: [Double]
baseIncomes = [1.0, 1.3, 0.7, 0.9]

startIncomes :: [Double]
startIncomes = [1,2,3,4]

calcIncome avgIncome =  map (*avgIncome) baseIncomes

calcHealth income  =  map (*10) income
{-income  = mkState startIncomes trans where
    trans dt (a, incomes) = (Right newstate, newstate) where
                                     newstate = map (* (average incomes)) baseIncomes
-}

info = proc _ -> do 
   rec income <- delay startIncomes . arr calcIncome -< avgIncome
       avgIncome <-  arr average -<  income
       health <- arr calcHealth -< income
   returnA -< (avgIncome, income, health)

wire :: WireP () String
wire = forI 20 . arr show . info

main = control return (putStrLn) $ wire