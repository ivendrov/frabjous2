-----------------------------------------------------------------------------
-- |
-- Module      :  Transform
-- Copyright   :  (c) University of Saskatchewan 2013
-- 
-- Maintainer  :  ivan.vendrov@usask.ca
--
-- A library of transformations on Haskell source code for use
-- in the Frabjous compiler. This is the only module in the compiler
-- that depends on a particular abstract syntax for Haskell
------------------------------------------------------------------------------
module Transform (linearizeDecl, prettifyDecl) where

import Language.Haskell.Exts.Parser -- needing for working with haskell source code
import qualified Language.Haskell.Exts.Pretty as Haskell -- needed for working with haskell source code
import qualified Language.Haskell.Exts.Syntax as Haskell
import Language.Haskell.Exts.Extension 




-- | converts a haskell declaration to an equivalent one-line declaration 
-- | by inserting braces and semicolons in accordance with the layout rules
linearizeDecl :: String -> Either String String
linearizeDecl = (fmap ppOneLine . readDecl) 

prettifyDecl :: String -> Either String String
prettifyDecl = (fmap ppPretty . readDecl) 

ppOneLine = Haskell.prettyPrintWithMode oneLineMode where
    oneLineMode = Haskell.defaultMode {Haskell.layout = Haskell.PPNoLayout}

ppPretty = Haskell.prettyPrintWithMode normal where
    normal = Haskell.defaultMode {Haskell.layout = Haskell.PPOffsideRule}

mode = defaultParseMode { extensions = [EnableExtension Arrows]}

readDecl:: String -> Either String Haskell.Decl
readDecl str = case parseDeclWithMode mode str of 
                 ParseOk decl -> Right decl
                 other -> Left (show other)


