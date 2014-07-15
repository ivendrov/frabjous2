-----------------------------------------------------------------------------
-- |
-- Module      :  Frabjous.Compiler.Transform
-- Copyright   :  (c) University of Saskatchewan 2013
-- 
-- Maintainer  :  ivan.vendrov@usask.ca
--
-- A library of transformations on Haskell source code for use
-- in the Frabjous compiler. This is the only module in the compiler
-- that depends on a particular abstract syntax for Haskell
------------------------------------------------------------------------------
module Frabjous.Compiler.Transform (linearizeDecl, prettifyDecl, desugarDecl) where

import Language.Haskell.Exts.Parser -- needing for working with haskell source code
import qualified Language.Haskell.Exts.Pretty as Haskell -- needed for working with haskell source code
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Extension 
import Data.Generics
import Control.Monad.State.Lazy
import Text.Printf (printf)




-- | converts a haskell declaration to an equivalent one-line declaration 
-- | by inserting braces and semicolons in accordance with the layout rules
linearizeDecl :: String -> Either String String
linearizeDecl = (fmap ppOneLine . readDecl) 

-- | converts a haskell declaration to an equivalent pretty-printed declaration 
prettifyDecl :: String -> Either String String
prettifyDecl = (fmap ppPretty . readDecl) 

-- | desugars reactive syntax used in a Haskell declaration and all subordinate where-clauses
-- | returns a linearized form
desugarDecl :: String -> Either String String
desugarDecl = (fmap (ppOneLine . desugar) . readDecl)

ppOneLine = Haskell.prettyPrintWithMode oneLineMode where
    oneLineMode = Haskell.defaultMode {Haskell.layout = Haskell.PPNoLayout}

ppPretty = Haskell.prettyPrintWithMode normal where
    normal = Haskell.defaultMode {Haskell.layout = Haskell.PPOffsideRule}

mode = defaultParseMode { extensions = [EnableExtension Arrows]}

readDecl:: String -> Either String Decl
readDecl str = case parseDeclWithMode mode str of 
                 ParseOk decl -> Right decl
                 other -> Left (show other)


-- | desugar reactive syntax
desugar :: Decl -> Decl 
desugar = everywhere (mkT desugarBinding)

type ReactiveState = (Int, [(String, Exp)])
type ReactiveMonad = State ReactiveState
getNum :: ReactiveMonad Int
getNum = gets fst

addRexp :: (String, Exp) -> ReactiveMonad ()
addRexp pair = modify (\(i, l) -> (i+1, pair : l))


desugarReactiveExp :: Exp -> ReactiveMonad Exp
desugarReactiveExp (App wire (Var (UnQual (Ident "t")))) = do 
  i <- getNum
  let str = printf "__%d" i
  addRexp (str, wire)
  return (Var (UnQual (Ident str)))
desugarReactiveExp exp = return exp
    
nullSrc = SrcLoc "" 0 0 -- TODO change to something bettwer
desugarRhs :: Rhs -> Rhs
desugarRhs (UnGuardedRhs rhs) = UnGuardedRhs $ Proc nullSrc (PVar (Ident "input")) (Do stmts) 
    where stmts = arrowBindings ++ [finalExp]
          arrowBindings = map toArrowBinding rexps
          toArrowBinding :: (String, Exp) -> Stmt
          toArrowBinding (str, exp) = Generator nullSrc 
                                      (PVar (Ident str))
                                      (LeftArrApp exp (Var (UnQual (Ident "input"))))
          finalExp = Qualifier $ LeftArrApp (Var (UnQual (Ident "returnA"))) newRhs
          (newRhs, (_ , rexps)) = runState (everywhereM (mkM desugarReactiveExp) rhs) (0, [])
desugarRhs rhs = rhs -- don't do anything to guarded rhs

desugarBinding :: Match -> Match
desugarBinding m@(Match src name patterns typ rhs binds) = if (last patterns == PVar (Ident "t"))
                                            then Match src name (init patterns) typ (desugarRhs rhs) binds
                                            else m



