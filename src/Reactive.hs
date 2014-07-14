{-# LANGUAGE NoMonomorphismRestriction #-}

import Language.Haskell.Exts.Parser -- needing for working with haskell source code
import qualified Language.Haskell.Exts.Pretty as Haskell -- needed for working with haskell source code
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Extension 
import Data.Generics
import Control.Monad.State.Lazy
import Text.Printf (printf)
{-
ppOneLine = Haskell.prettyPrintWithMode oneLineMode where
    oneLineMode = Haskell.defaultMode {Haskell.layout = Haskell.PPNoLayout}

ppPretty = Haskell.prettyPrintWithMode normal where
    normal = Haskell.defaultMode {Haskell.layout = Haskell.PPOffsideRule}
-}
mode = defaultParseMode { extensions = [EnableExtension Arrows]}

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

desugar :: Decl -> Decl 
desugar = everywhere (mkT desugarBinding)

ppPretty = Haskell.prettyPrintWithMode normal where
    normal = Haskell.defaultMode {Haskell.layout = Haskell.PPOffsideRule}

desugarString :: String -> Either String String
desugarString = fmap (ppPretty . desugar) . readDecl

main = do
  str <- getLine
  let (Right s) = desugarString str
  putStrLn s

readDecl:: String -> Either String Decl
readDecl str = case parseDeclWithMode mode str of 
                 ParseOk decl -> Right decl
                 other -> Left (show other)