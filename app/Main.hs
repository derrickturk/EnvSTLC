{-# LANGUAGE DataKinds, OverloadedStrings #-}

module Main where

import Language.EnvSTLC.Syntax
import Language.EnvSTLC.Eval

-- (\x:Bool . if x then 3 else 4)(true && true)
aTerm :: Term 'Checked
aTerm = App
  (Lam "x" BoolTy (IfThenElse (Var "x") (IntLit 3) (IntLit 4)))
  (And (BoolLit True) (BoolLit True))

main :: IO ()
main = do
  let _ = eval aTerm
  print $ eval aTerm
