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
  isEOF >>= \e -> unless e (execLine >> main)
  where
    execLine = do
      line <- TIO.getLine
      let parsed = P.parse P.term "stdin" line
      case parsed of
        Left err -> putStrLn $ P.parseErrorPretty err
        Right term -> do
          putStr "parsed: "
          print term
          case typecheck term of
            Right term' -> do
              let (v, env) = evalEnv term'
              putStr "value: " 
              print v
              putStr "final env: " 
              print env
              putStrLn ""
            Left e -> print e >> putStrLn ""
