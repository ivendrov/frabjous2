-----------------------------------------------------------------------------
-- |
-- Module      :  Analyzer
-- Copyright   :  (c) University of Saskatchewan 2013
-- 
-- Maintainer  :  ivan.vendrov@usask.ca
--
-- Performs semantic analysis of Frabjous abstract syntax
-----------------------------------------------------------------------------
module Analyzer where

import qualified Syntax as S


type HaskellBlock = S.HaskellBlock
type Annotation = String

data Agent = Agent { name :: String,
                     attributeNames :: [(String, Annotation)], -- names of all attributes
                     nonNetworkAttributes :: [Attribute]}  -- attributes with actual code
data Population = Population { populationName :: String, 
                               agent :: String,
                               removal :: HaskellBlock,
                               addition :: HaskellBlock }
data Network = Network { population1 :: (String, String),
                         population2 :: Maybe (String, String),
                         networkSpec :: HaskellBlock }
data Attribute = Attribute { varName :: String,
                             code :: HaskellBlock}

                         
                         
-- TODO differentiate between global and local variables; possibly give each network
-- an identifying index, other useful syntactic features, etc                             
data FrabjousModel = FrabjousModel { agents :: [Agent],
                                     attributes :: [Attribute],
                                     localAttributeNames :: [String],
                                     globals :: [Attribute],
                                     populations :: [Population],
                                     networks :: [Network],
                                     otherCode :: String}
      

-- TODO add error conditions
analyze :: S.Program -> FrabjousModel
analyze = analyzeAttributes . foldr analyzeBlock (FrabjousModel [] [] [] [] [] [] "") . S.blocks

-- adds a Frabjous program block to the model, without doing any "hooks"
analyzeBlock ::  S.Block -> FrabjousModel -> FrabjousModel
analyzeBlock (S.AgentDec agentName fields) model = 
    model { agents = (Agent agentName fields []) : agents model, 
            localAttributeNames = map fst fields ++ localAttributeNames model}
analyzeBlock (S.VariableDec varname code) model = model { attributes = (Attribute varname code) : attributes model }
analyzeBlock (S.PopulationDec pname agent removal addition) model = 
                  model { populations = (Population pname agent removal addition) : populations model }
analyzeBlock (S.NetworkDec pop1 pop2 networkSpec) model = 
    model { networks = (Network pop1 pop2 networkSpec) : networks model }
analyzeBlock (S.JustHaskell (S.HaskellBlock contents)) model = 
    model {otherCode = unlines [contents,otherCode model]}

-- hook agents up to their attributes
-- TODO detect global attributes, and add a blank "never" for 
-- every undeclared attribute
analyzeAttributes :: FrabjousModel -> FrabjousModel
analyzeAttributes model =  model { agents = map addAttributes . agents $ model} 
    where 
      addAttributes :: Agent -> Agent
      addAttributes agent = agent { nonNetworkAttributes = attributesNamed (map (fst) . attributeNames $ agent) }
      attributesNamed :: [String] -> [Attribute]
      attributesNamed names = filter ((`elem` names) . varName) (attributes model)
        
