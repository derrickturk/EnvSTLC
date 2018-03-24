{-# LANGUAGE DataKinds, OverloadedStrings #-}

module Main where

import Language.EnvSTLC.Syntax()
import qualified Language.EnvSTLC.Parser as P
import Language.EnvSTLC.Typecheck
import Language.EnvSTLC.Eval
import qualified Data.Text.IO as TIO
import Control.Monad (unless)
import System.IO (isEOF)

main :: IO ()
main = do
  execLine
  isEOF >>= \e -> unless e main
  where
    execLine = do
      line <- TIO.getLine
      let parsed = P.parse P.term "stdin" line
      case parsed of
        Left err -> putStr $ P.parseErrorPretty err
        Right term -> do
          print term
          case typecheck term of
            Right term' -> print $ eval term'
            Left e -> print e
