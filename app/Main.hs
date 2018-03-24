{-# LANGUAGE DataKinds, OverloadedStrings #-}

module Main where

import Language.EnvSTLC.Syntax()
import qualified Language.EnvSTLC.Parser as P
import Language.EnvSTLC.Typecheck
import Language.EnvSTLC.Eval
import qualified Data.Text as T

-- (\x:Bool . if x then 3 else 4)(true && true)
aTerm :: T.Text
aTerm = "(\\x : Bool . if x then 3 else 4)(true && true)"

main :: IO ()
main = do
  let parsed = P.parse P.term "built-in" aTerm
  case parsed of
    Left err -> print err
    Right term -> do
      print term
      case typecheck term of
        Right term' -> print $ eval term'
        Left e -> print e
