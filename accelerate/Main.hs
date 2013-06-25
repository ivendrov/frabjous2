module Main (main) where
import System.Environment(getArgs)
import Data.Array.Accelerate (toList, shape)
import Model
import GameOfLife


main = do
        args <- getArgs
        putStrLn . show $  runModel (gameOfLife (read $ args !! 0) (read $ args !! 0)) 2
  
