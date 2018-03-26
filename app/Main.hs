{-# LANGUAGE DataKinds, OverloadedStrings #-}

module Main where

import Language.EnvSTLC.Syntax
import qualified Language.EnvSTLC.Parser as P
import Language.EnvSTLC.Environment
import Language.EnvSTLC.Typecheck
import Language.EnvSTLC.Eval
import qualified Data.Text.IO as TIO
import Control.Monad (unless)
import System.IO (isEOF)
import Control.Monad.State.Strict
import Control.Monad.Except

main :: IO ()
main = execLine emptyS emptyE emptyE

execLine :: Scope -> TypeEnv -> TermClosureEnv -> IO ()
execLine s typeEnv termEnv = isEOF >>= \eof -> unless eof $ do
  line <- TIO.getLine
  let parsed = P.parse (P.only P.replItem) "stdin" line
  case parsed of
    Left err -> do
      putStrLn $ P.parseErrorPretty err
      execLine s typeEnv termEnv
    Right (ReplTerm term) -> do
      putStr "parsed: "
      print term
      case evalState (runExceptT $ typecheckM $ Closure s term) typeEnv of
        Right term' -> do
          let (v, termEnv') = runState (evalM $ Closure s term') termEnv
          putStr "value: " 
          print v
          putStr "updated env: " 
          print termEnv'
          putStrLn ""
          execLine s typeEnv termEnv'
        Left e -> do
          print e
          putStrLn ""
          execLine s typeEnv termEnv
    Right (ReplStmt stmt) -> do
      putStr "parsed: "
      print stmt
      case runState (runExceptT $ typecheckStmtM $ Closure s stmt) typeEnv of
        (Right (s', stmt'), typeEnv') -> do
          putStrLn "got a thing"
          putStrLn ""
          execLine s' typeEnv' termEnv
        (Left e, _) -> do
          print e
          putStrLn ""
          execLine s typeEnv termEnv
