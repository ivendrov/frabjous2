-- testing out TH

{-# LANGUAGE TemplateHaskell #-}

module Foo where

import Language.Haskell.TH

mkCross n = do
	xs <- names n "x"
	ls <- names n "l"
	let aux (x,l) = BindS (VarP x) (VarE l)
	let xls = map aux (xs `zip` ls)
	return $ LamE  (map VarP ls) (CompE (xls ++ [NoBindS (TupE (map VarE xs))]))
		
		
names :: Int -> String -> Q [Name]
names 0 _ = return []
names (n+1) str = do
			x <- newName str
			xs <- names n str
			return $ x:xs

