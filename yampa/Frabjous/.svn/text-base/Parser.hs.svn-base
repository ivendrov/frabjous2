-- Parser.hs
--
-- parser for the Frabjous system
--
-- Oliver Schneider
-- University of Saskatchewan


-- NOTE: 	In this parser I use the state in the parser to
--			keep track of indentation.


module Frabjous.Parser (parseFrabjous, parseFrabjousFile) where


import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Prim  hiding (State)
import Text.ParserCombinators.Parsec.Pos
import Data.Maybe
import Frabjous.Base


--Top level specification
frabspec = do
			tree <- modelHead 
			nextline
			tree' <- modelBody tree
			spaces
			eof
			return tree'

-- model header
--
-- of the form: [continuous|discrete] model <name>
modelHead = do
				let disc = string "discrete" >> return Discrete
				let cont = string "continuous" >> return Continuous
				let envDef = disc <|> cont <|> fail "invalid model header"
				spaces
				env <- envDef
				spaces1
				string "model"
				spaces1
				name <- identifier
				return $ Model name env [] [] [] [] []
			
--
modelBody tree = do
				tree' <- optionMaybe $ modelDec tree
				r <- maybe (return tree) modelBody tree' --not tail recursive?
				return r
modelDec tree =  try (messageDec tree)
				<|> try (networkDec tree)
				<|> try (diagramDec tree)
				<|> try (agentHead tree)
				<|> try (confoundHead tree)
				<|> fail "invalid model item"
				
--tree@(Model name env msgs nets diags confs agents)

--base level message declaration
messageDec (Model name env msgs nets diags confs agents) = do
				spaces
				m <- message (-1)
				pos' <- getPosition
				return $ Model name env (msgs ++ [m]) nets diags confs agents

-- message description
-- 
-- must be indented further in than n
--
-- of the form: on <trigger> send <string> to <target>
--TODO: more flexible syntax		
message n = do
		spaces
		checkIndentation n
		string "on"
		spaces1
		trig <- trigger
		spaces1
		string "send"
		spaces1
		char '"'
		payload <- manyTill (noneOf "\t\n") (char '"')
		spaces
		string "to"
		spaces1
		ts <- target
		return $ Msg trig ts payload [] --TODO: Read variables
			
-- definition of trigger
trigger = 	startup
			<|> timeout
			<|> enter
			<|> leave
			<|> try rate
			<|> receive
	where
		startup = string "startup" >> return Startup
		rate = string "rate" >> spaces1 >> getDouble >>= (\x -> return (Rate x) )
		timeout = string "timeout" >> spaces1 >> getDouble >>= (\x -> return (Timeout x))
		enter = do
			string "enter"
			spaces1
			f <- identifier
			rest <- (char ':' >> identifier) <|> string ""
			return $ if rest /= "" then EnterState (f++":"++rest) else EnterState f
		leave = do
				string "leave"
				spaces1
				f <- identifier
				rest <- (char ':' >> identifier) <|> string ""
				return $ if rest /= "" then LeaveState (f++":"++rest) else LeaveState f
		receive = do
				string "receive"
				spaces1
				char '"'
				m <- manyTill (noneOf "\t\n") (char '"')
				return $ Receive m



-- definition of target
--PLACEHOLDER
target = 	anyone 
			<|> everyone
			<|> try neighbour
			<|> neighbours
			<|> fail "invalid target"
	where
		anyone = string "anyone" >> return Anyone
		everyone = string "everyone" >> return Everyone
		neighbour = string "neighbour" >> notFollowedBy (char 's') >> spaces >> identifier >>= \x -> return $ Neighbour x
		neighbours = string "neighbours" >> return Neighbours >> spaces >> identifier >>= \x -> return $ Neighbour x
		
--target = sepBy1 filt (try (spaces >> char '>' >> spaces))

--PLACEHOLDER for filter
--filt = string "filt" >> optional digit >> return FAll
	--should probably define these more before implementing
	{- self
			<|> all
			<|> random
			<|> net
--			<|> rweight
			<|> first
			<|> ftake
			<|> fname
--			<|> expr -}

-- definition of network structure
networkStructure =	distance
					<|> random 
					<|> try moore
					<|> vn
					<|> manhattan
					<|> fail "invalid network structure"
		where
			distance = string "distance" >> spaces1 >> getDouble >>= (\x -> return (ByDistance x))
			random = string "random" >> spaces1 >> getDouble >>= (\x -> return (Random x))
			moore = string "moore" >> return Moore
			-- vn = oneOf "Vv" >> string "on" >> spaces >> oneOf "Nn" >> string "eumann" >> return VonNeumann
			vn = string "von" >> spaces >> string "neumann" >> return VonNeumann
			manhattan = string "manhattan" >> spaces1 >> getInt >>= (\x -> return (Manhattan x))

--PLACEHOLDER
varDec = string "variable" >> return (VInt (-1))

-- definition of transition
transition = do --string "transition" >> return (Transition "transition" [] "switchTo")
		string "on"
		spaces1
		trig <- trigger
		spaces1
		string "switch"
		spaces1
		string "to"
		spaces1
		targ <- identifier
		return $ Transition trig targ


-- network declaration
--
-- of the form: network <name> of <agent-name> by <structure>
networkDec (Model name env msgs nets diags confs agents) = do
				spaces
				string "network"
				spaces1
				n <- identifier
				spaces1
				string "of"
				spaces1
				agent_name <- identifier
				spaces1
				string "by"
				spaces1
				nType <- networkStructure
				let net = Network n agent_name nType
				return $ Model name env msgs (nets ++ [net]) diags confs agents

-- agent header
--
-- of the form:	agent <name> with
--					<diagram>
--					<confounder>
--					[population <int>]  (with default 1)
agentHead (Model name env msgs nets diags confs agents) = do
				spaces
				pos <- getPosition
				let i = sourceColumn pos
				string "agent"
				spaces1
				n <- identifier
				spaces1
				string "with"
				nextline
				agent <- agentBody i (Agent n [])
				return $ Model name env msgs nets diags confs (agents ++ [agent])
				
--indented body of an agent description
agentBody i agent = do
				agent' <- optionMaybe $ try $ agentDec i agent
				r <- maybe (return agent) (agentBody i) agent'
				return r

agentDec i (Agent name props) = do
				spaces
				checkIndentation i
				let pop = string "population" >> spaces1 >> many digit
				let decs = try pop <|> identifier <|> fail "invalid agent description"
				r <- decs
				return $ Agent name (props ++ [r])


-- confounder header
--
-- of the form: agent <diagram-name>* with
--					<msg>
--					<msg>
confoundHead (Model name env msgs nets diags confs agents) = do
				spaces
				pos <- getPosition
				let i = sourceColumn pos
				string "confound"
				spaces1
				ns <- manyTill (spaces >> identifier) (try (spaces1 >> string "with" >> nextline))
				cmsgs <- messages i
				let conf = Confounder ns cmsgs
		 		return $ Model name env msgs nets diags (confs ++ [conf]) agents


-- diagram declaration
--
-- of the form: diagram <name> [starting with <state-name>]
--					<state>
--					<state>
--					...
diagramDec (Model name env msgs nets diags confs agents) = do
		spaces
		pos <- getPosition
		let i = sourceColumn pos
		string "diagram"
		spaces1
		n <- identifier
		let startDec = spaces1 >> string "starting" >> spaces1 >> string "with" >> spaces1 >> identifier
		startWith <- try startDec <|> string ""
		nextline
		states <- many1 $ try $ stateDec i
		let startWith' = if startWith == "" then head (map getName states) else startWith
		let diag = Diagram n startWith' states
		return $ Model name env msgs nets (diags ++ [diag]) confs agents
		

-- state declaration
--
-- of the form: state <name> displays <colour>
--					<msg>
--					<msg>
--					...
stateDec n = do
		spaces
		checkIndentation n
		pos <- getPosition
		let i = sourceColumn pos
		string "state"
		spaces1
		name <- identifier
		spaces1
		string "displays"
		spaces1
		col <- many letter
		nextline
		let st = State name [] [] [] col
		st' <- stateBody i st
		return st'
		
stateBody n st = do
			st' <- optionMaybe $ stateItem n st
			r <- maybe (return st) (stateBody n) st' --not tail recursive?
			return r

stateItem n (State name msgs vars trans disp) =	try stateMessage
												<|> try stateVar
												<|> try stateTransition
												<|> fail "invalid state item"
					where
						stateMessage = do
							msg <- message n
							return $ State name (msgs ++ [msg]) vars trans disp
						stateVar = do
							spaces
							checkIndentation n
							var <- varDec
							return $ State name msgs (vars ++ [var]) trans disp
						stateTransition = do
							spaces
							checkIndentation n
							tran <- transition
							return $ State name msgs vars (trans ++ [tran]) disp


			


checkIndentation n = do
				pos <- getPosition
				let valid = (sourceColumn pos) > n
				if not valid then fail "invalid indentation" else string ""
			

-- parses a list of indented messages (that are indented further than n)
messages n = many $ try $ message n
				
				

-- one or more spaces		
spaces1 = space >> spaces

-- consume space up to and including a newline
nextline = manyTill space newline

-- gets a valid identifier for anything inside 
identifier = do
			f <- lower <|> fail "identifier must start with a lowercase letter"
			rest <- many (alphaNum <|> char '_') <|> fail "invalid character in identifier"
			return $ f:rest

getDouble = do
		f <- digit <|> oneOf "-."
		rest <- many $ digit <|> char '.'
		return ((read (f:rest)) :: Double)

getInt = do
		f <- digit <|> char '-'
		rest <- many digit
		return ((read (f:rest)) :: Int)


parseFrabjous :: String -> Either ParseError TModel
parseFrabjous = parse frabspec "unknown parse error"

--parseFrabjousFile :: String -> IO (Either ParseError TModel)
parseFrabjousFile = parseFromFile frabspec
		
		



