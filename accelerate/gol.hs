import Data.Array.Accelerate as A
import Data.Array.Accelerate.CUDA as CUDA
import Data.List as List


type Agent = Bool 
type AgentArray = Array DIM1 Agent


width = 3
height = 3

allcells = [(x,y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]

-- convert a 2D index of a matrix into the corresponding index for a flattened array
-- if arr is a matrix of dimensions width x height, then for all i, 
-- (flatten arr) ! (indexFlatten i) = arr ! i 
indexFlatten :: (Int, Int) -> Int
indexFlatten (i,j) = width * i + j



-- the initial game of life board
initBoard :: [[Agent]] 
initBoard = [[True,True,False], 
	     [True,False,False], 
	     [False,False,False]]



-- returns the neighbours of the given point on an 8-connected grid
neighbours8 :: (Int, Int) -> [(Int, Int)]
neighbours8 (x, y) = [(i, j) | i <- [x-1 .. x+1], 
					 j <- [y-1 .. y+1],
					 Prelude.not (i == x && j == y),
                     inBounds i j] 
                    where inBounds i j = (i >= 0) &&
                                         (j >= 0) &&
                                         (i < width) &&
                                         (j < height)
                     
-- adjacency list representation of the neighbour network of game of life
adjlist = List.map (List.map indexFlatten . neighbours8) allcells



-- a network is an flattened adjacency list, and an array storing 
-- the number of connections for each agent
data Network = Network { adjList :: (Array DIM1 Int),
			 segments :: (Array DIM1 Int)}
    deriving (Show)


networkFromAdjList adj = Network (fromList (Z:.m) cadg) (fromList (Z:.n) $ List.map (length) adj)
	where cadg = concat adj
	      m = length cadg -- number of edges / connections
	      n = length adj -- number of vertices / agents

agentArray = fromList (Z:.n) (concat initBoard) 
	where n = width * height

network = networkFromAdjList adjlist



step :: Network -> Acc (AgentArray)-> Acc (AgentArray)
step (Network adj segs) agents = A.zipWith nextCell agents connections
	where nextCell currentVal liveCells = 
		currentVal ? ((liveCells ==* 2) ||* (liveCells ==* 3), liveCells ==* 3)
	      connections = foldSeg (+) 0 (A.map boolToInt adjvals) segs'
	      boolToInt :: Exp Bool -> Exp Int
              boolToInt b = b ? (1,0)
	      adjvals = backpermute (shape adj')
			 	    (\i -> index1 $ adj' ! i)
				    agents
	      adj' = use adj
              segs' = use segs

run :: Network -> AgentArray -> Int -> AgentArray
run network agents steps = CUDA.run $ helper (use agents) steps 
	where helper agents 0 = agents 
	      helper agents n = helper (step network agents) (n-1)
	



{-
compute n s = CUDA.run $ A.fold (+) 0 (use arr) 
            where arr  = fromList (Z:.n) [s..] :: Array DIM1 Int

main = do
        str <- getLine
        putStrLn $ show $ {-Prelude.sum $ Prelude.map (A.! 0) $-} Prelude.map (compute (read str)) [1]
-}
