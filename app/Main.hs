{-# LANGUAGE DataKinds, OverloadedStrings #-}

module Main where

import Language.EnvSTLC.Syntax
import Language.EnvSTLC.Typecheck
import Language.EnvSTLC.Eval

-- (\x:Bool . if x then 3 else 4)(true && true)
aTerm :: Term 'Unchecked
aTerm = App
  (Lam "x" IntTy (IfThenElse (Var "x") (IntLit 3) (IntLit 4)))
  (And (BoolLit True) (BoolLit True))

main :: IO ()
main = do
  print aTerm
  case typecheck aTerm of
    Right aTerm' -> print $ eval aTerm'
    Left e -> print e
