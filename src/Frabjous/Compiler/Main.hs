-----------------------------------------------------------------------------
-- |
-- Module      :  Frabjous.Compiler.Main
-- Copyright   :  (c) University of Saskatchewan 2013
-- 
-- Maintainer  :  ivan.vendrov@usask.ca
--
-- The entry point to the Frabjous Compiler
------------------------------------------------------------------------------

-- Frabjous imports
import Debug.Trace
import Frabjous.Compiler.Syntax
import qualified Frabjous.Compiler.Parser as Parser
import qualified Frabjous.Compiler.CodeGen as CodeGen



main = do
  contents <- getContents
  putStrLn (process contents)

-- given the input lines of the Frabjous program, generate Haskell code
process :: String -> String
process str = case Parser.parseProgram str of
                Left err -> show err
                Right ast -> CodeGen.generateCode ast

                                 

                                            
              
            
            
                                 



  