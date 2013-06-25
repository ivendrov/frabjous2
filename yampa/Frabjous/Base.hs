-- Base.hs
--
-- core definitions for the Frabjous system
--
-- Oliver Schneider
-- University of Saskatchewan


module Frabjous.Base where

import Data.List (sort, find)
import Data.Maybe (maybeToList)
import Data.Char (isDigit)



----------------------
-- Data Definitions --
----------------------

-- Named type class
class Named a where
	getName :: a -> String 

-- Top level Model Structure
data TModel = Model String
					TEnv
					[TMsg]
					[TNetwork]
					[TDiagram]
					[TConfounder]
					[TAgent]
	deriving(Show)

instance Named TModel where
	getName (Model n _ _ _ _ _ _ ) = n


-- Agent structure
data TAgent = Agent String		--name
					[String]	--diagrams/confounders/population
	deriving(Show)

instance Named TAgent where
	getName (Agent n _) = n


-- Diagram structure
data TDiagram =		Diagram
					String		--name
					String		--starting state
					[TState]	-- states
	deriving(Show)

instance Named TDiagram where
	getName (Diagram n _ _ ) = n


-- State structure
data TState	=	State	String	--name
						[TMsg]
						[TVar]
						[TTransition]
						TDisplay
	deriving(Show)

instance Named TState where
	getName (State n _ _ _ _ ) = n


-- Message structure
data TMsg	=	Msg	TTrig
					TTarget
					String
					[TVar] -- currently not really used
	deriving(Show)


-- Trigger structure
-- TODO: Flesh out
data TTrig	=	Startup	
				| Rate Double 
				| Timeout Double
			--	| ExprT BoolExpr how do I do this?
				| EnterState String
				| LeaveState String
				| Receive String
	deriving(Show)


-- Variable structure
-- TODO: Flesh out (add name?
data TVar	=	VInt Int
				| VString String
				| VDouble Double
	deriving(Show)


-- Transition structure
data TTransition	= Transition	TTrig 	-- should this just be a message?
--									[TVar]	-- ???
									String	-- target state
	deriving(Show)

-- Target structure
--PLACEHOLDER
data TTarget =	Anyone
				| Everyone
				| Neighbour String --name of network
				| Neighbours String -- name of network
	deriving(Show, Eq)
--type TTarget = [TFilter]


-- Filter structure
-- TODO: flesh this out
{- data TFilter = 	FSelf 
				| FAll
				| FRandom
				| FNetwork String
			--	| FRandomByWgt
				| FFirst Int
				| FName String
			--	| FExpr BoolExpr
	deriving(Show)
-}

-- Boolean expression
-- TODO: Figure out how the heck I can do this
-- type BoolExpr = [TVar] -> Bool
type BoolExpr = String


-- Confounder structure
data TConfounder = Confounder	[String]	-- diag names
								[TMsg]
	deriving(Show)


-- Network structure
data TNetwork = Network	String	-- name
						String	-- agentType
						TStructure 
	deriving(Show)

instance Named TNetwork where
	getName (Network n _ _ ) = n


-- network structure definition
data TStructure =	ByDistance Double	-- distance
					| Random Double		-- connectivity between 0 and 1
					| Moore
					| VonNeumann
					| Manhattan Int
	deriving(Show)


-- model environment type
data TEnv = Discrete | Continuous
	deriving(Show)


-- display information
-- TODO: flesh out
type TDisplay = String




--------------------
-- Error Checking --
--------------------


type ErrorMessage = String

coherencyCheck :: TModel -> [ErrorMessage]
coherencyCheck (Model name env msgs nets diags confs agents) = chkDups ++ chkDiags ++ chkNets ++ chkTargets ++ chkAgents -- ++ chkConfounds
		where
			chkDups = map ambigError $ dups $ names
			ambigError n = "Error: ambiguous name \"" ++ n ++ "\"."
			names = (map getName nets) ++ (map getName diags) ++ (map getName agents)
			chkDiags = concatMap chkDiag diags
			chkDiag (Diagram name start states) = chkStarter ++ chkStates
				where
					chkStarter = case find (==start) (map getName states) of
										Nothing -> ["Error: Could not find starting state \"" ++ start ++ "\""]
										Just x -> []
					chkStates = map ambigError $ dups $ map getName $ states
			chkNets = concatMap chkNet nets
			chkNet (Network name agent struct) = chkAgent ++ (chkStruct env struct)
				where
					chkAgent = maybe [findAgentError] (const []) $ (find (==agent) $ map getName agents)
					findAgentError = "Error: Network \"" ++ name ++ "\" references non-existent agent \"" ++ agent ++ "\"."
					chkStruct Continuous Moore = ["Error: Continuous model cannot support Moore neighbourhood in network \"" ++ name ++ "\"."]
					chkStruct Continuous VonNeumann = ["Error: Continuous model cannot support Von Neumann neighbourhood in network \"" ++ name ++ "\"."]
					chkStruct Continuous (Manhattan _) = ["Error: Continuous model cannot support Manhattan neighbourhood in network \"" ++ name ++ "\"."]
					chkStruct _ (Manhattan x) = if x < 0 then ["Error: Manhattan distance must be non-negative in network \"" ++ name ++ "\"."] else []
					chkStruct _ _ = []
			chkTargets = chkBaseTargets ++ chkStateTargets -- ++ chkConfTargets
				where
					chkBaseTargets = concatMap chkBaseTarget msgs
					chkBaseTarget (Msg _ targ _ _) = 	if (targ == Anyone || targ == Everyone)
															then []
															else ["Error: Model-level messages cannot rely on networks."]
					chkStateTargets = concatMap chkStateTarget diags
					chkStateTarget (Diagram dn _ states) = concatMap f states
						where
							f (State sn ms _ _ _) = concatMap f' ms
								where
									f' (Msg _ (Neighbour s) _ _)  = maybe [f'err s] (const []) $ find (==s) $  map getName nets
									f' (Msg _ (Neighbours s) _ _) = maybe [f'err s] (const []) $ find (==s) $  map getName nets
									f' _ = []
									f'err s = "Error: state \"" 
												++ sn 
												++ "\" in diagram \"" 
												++ dn 
												++ "\" has message referencing non-existent network \"" 
												++ s 
												++ "\"."
			chkAgents = concatMap chkAgent agents
			chkAgent (Agent name contents) = chkPops ++ chkOtherContents
					where
						chkPops = if (length pops) > 1 
									then ["Error: more than one population specified for agent " ++ name]
									else chkNeg pops
						pops =  map readInt $ filter isNumber contents
						chkNeg =  maybe [] (const [negError]) . find (< 0)
						negError = "Error: negative populations specified for agent " ++ name
						chkOtherContents = [] --PLACEHOLDER STUB!
						
						
{-
				
maybe [] 
												let r = noDups names
												Nothing ->  Right must s -> Left $ "Error: ambiguous name \"" ++ s ++ "\"."
				where
					chkDups = maybe [] (\n
-}					
					

-- returns a list of all duplicates in a list
--
-- O(n log n)

dups :: Ord a => [a] -> [a] -- Should make this for Eq a but this will be faster (n log n)
dups = dups' . sort
	where
		dups' [] = []
		dups' [_] = []
		dups' (x:y:rest) = if (x == y) then x:(dups' (dropWhile (==x) rest)) else dups' (y:rest)

readInt :: String -> Int
readInt = read

isNumber :: String -> Bool
isNumber = isDigit . head

--------------------
-- IMPLEMENTATION --
--------------------

-- type StateRep = String 

-- type RepToState = [ (StateRep, StateSF) ]
