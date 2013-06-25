module Model where 

import Data.Array.Accelerate as A
--import Data.Array.Accelerate.CUDA as CUDA
import Data.Array.Accelerate.Interpreter as Interpreter
import Data.List as List


{- GENERAL MODEL DEFINITIONS -}

-- a network is an flattened adjacency list, and an array storing 
-- the number of connections for each agent
data Network = Network { adjList :: (Array DIM1 Int),
			 segments :: (Array DIM1 Int)}
    deriving (Show)


type TransitionFunction agent = Network -> Acc (Array DIM1 agent) -> Acc (Array DIM1 agent)


data Model agent = Model {network :: Network, 
			  step :: TransitionFunction agent,
			  initialAgents :: Array DIM1 agent }

runModel (Model network step init) n = helper n init where
	helper 0 init = init
	helper n init = helper (n-1) $ Interpreter.run $ step network (use init)



