import Data.Array.Accelerate as A
import Data.Array.Accelerate.CUDA as CUDA
compute :: Int -> Int -> Array DIM0 Int
compute n s = CUDA.run $ A.fold (+) 0 (use arr) 
            where arr  = fromList (Z:.n) [s..] :: Array DIM1 Int

main = do
        str <- getLine
        putStrLn $ show $ {-Prelude.sum $ Prelude.map (A.! 0) $-} Prelude.map (compute (read str)) [1]

