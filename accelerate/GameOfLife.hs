module GameOfLife (gameOfLife, pretty) where 

import Data.Array.Accelerate as A
import Data.List as List
import Data.List.Split (chunksOf)
import Model


pretty :: Int -> Array DIM1 Bool -> String
pretty width = unlines . chunksOf width . List.map (star) . toList
	where star True = '*'
	      star False = ' '

-- returns a game of life board of size width x height
gameOfLife :: Int -> Int -> Model Bool 
gameOfLife width height = if (width < 3 || height < 3) 
			  then error "dimensions too small"
			  else Model network step initialAgents
       where
	allcells = [(x,y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]

	-- convert a 2D index of a matrix into the corresponding index for a flattened array
	-- if arr is a matrix of dimensions width x height, then for all i, 
	-- (flatten arr) ! (indexFlatten i) = arr ! i 
	indexFlatten :: (Int, Int) -> Int
	indexFlatten (i,j) = width * i + j



	-- the initial game of life board
	initBoard :: [[Bool]] 
	initBoard = 	[[False, True, True] ++ (List.replicate (width-3) False), 
	     		 [True, True, False] ++ (List.replicate (width-3) False), 
	     		 [False, True, False]++ (List.replicate (width-3) False)]
                        ++ List.replicate (height-3) (List.replicate width False)



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






	networkFromAdjList adj = Network (fromList (Z:.m) cadg) (fromList (Z:.n) $ List.map (length) adj)
		where cadg = concat adj
		      m = length cadg -- number of edges / connections
		      n = length adj -- number of vertices / agents

	initialAgents = fromList (Z:.n) (concat initBoard) 
		where n = width * height

	network = networkFromAdjList adjlist


 		


	step :: Network -> Acc (Array DIM1 Bool)-> Acc (Array DIM1 Bool)
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

