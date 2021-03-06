{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.EnvSTLC.Repl (
    ReplItem
  , ReplState(..)
  , Repl
  , initialState
  , runRepl
  , evalRepl
  , execRepl
  , runRepl_
  , updateTypeScope
  , updateTermScope
  , updateTypeEnv
  , updateTermEnv
  , replTypecheckStmt
  , replTypecheckTerm
  , replExecStmt
  , replEvalTerm
  , replLine
  , replLoop
  , replPutStr
  , replPutStrLn
  , replPrint
  , replPrompt
) where

import Language.EnvSTLC.Syntax
import Language.EnvSTLC.Environment
import Language.EnvSTLC.Typecheck
import Language.EnvSTLC.Eval
import qualified Language.EnvSTLC.Parser as P
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import System.IO.Error (isEOFError, catchIOError, ioError)
import System.Exit (exitSuccess)

data ReplState = ReplState { typeScope :: Scope
                           , termScope :: Scope
                           , typeEnv :: TypeEnv
                           , termEnv :: TermClosureEnv
                           } deriving Show

newtype Repl a = Repl { unRepl :: ExceptT TypeError (StateT ReplState IO) a }
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadError TypeError
    , MonadState ReplState
    , MonadIO
  )

initialState :: ReplState
initialState = ReplState [] [] emptyE emptyE

runRepl :: Repl a -> ReplState -> IO (Either TypeError a, ReplState)
runRepl (Repl r) s = runStateT (runExceptT r) s

evalRepl :: Repl a -> ReplState -> IO (Either TypeError a)
evalRepl (Repl r) s = evalStateT (runExceptT r) s

execRepl :: Repl a -> ReplState -> IO ReplState
execRepl (Repl r) s = execStateT (runExceptT r) s

runRepl_ :: Repl a -> ReplState -> IO ()
runRepl_ (Repl r) s = () <$ runStateT (runExceptT r) s

updateTypeScope :: Scope -> Repl ()
updateTypeScope s = modify (\r -> r { typeScope = s })

updateTermScope :: Scope -> Repl ()
updateTermScope s = modify (\r -> r { termScope = s })

updateTypeEnv :: TypeEnv -> Repl ()
updateTypeEnv e = modify (\r -> r { typeEnv = e })

updateTermEnv :: TermClosureEnv -> Repl ()
updateTermEnv e = modify (\r -> r { termEnv = e })

-- I'm not smart enough to auto-lift my actions on
--   inner state up to the REPL monad, so I'll do it by hand.

replTypecheckStmt :: Stmt 'Unchecked -> Repl (Stmt 'Checked)
replTypecheckStmt stmt = Repl $ do
  env <- typeEnv <$> get
  s <- typeScope <$> get
  case runState (runExceptT $ typecheckStmtM $ Closure s stmt) env of
    (Right (s', stmt'), env') -> do
      unRepl $ updateTypeScope s'
      unRepl $ updateTypeEnv env'
      return stmt'
    (Left e, _) -> throwError e

replTypecheckTerm :: Term 'Unchecked -> Repl (Term 'Checked)
replTypecheckTerm term = Repl $ do
  env <- typeEnv <$> get
  s <- typeScope <$> get
  case runState (runExceptT $ typecheckM $ Closure s term) env of
    (Right term', env') -> do
      unRepl $ updateTypeEnv env'
      return term'
    (Left e, _) -> throwError e

replExecStmt :: Stmt 'Checked -> Repl ()
replExecStmt stmt = do
  env <- termEnv <$> get
  s <- termScope <$> get
  let (s', env') = runState (execM $ Closure s stmt) env
  updateTermScope s'
  updateTermEnv env'

replEvalTerm :: Term 'Checked -> Repl Value
replEvalTerm term = do
  env <- termEnv <$> get
  s <- termScope <$> get
  let (v, env') = runState (evalM $ Closure s term) env
  updateTermEnv env'
  return v

replPutStr :: String -> Repl ()
replPutStr = liftIO . putStr

replPutStrLn :: String -> Repl ()
replPutStrLn = liftIO . putStrLn

replPrint :: Show a => a -> Repl ()
replPrint = liftIO . print

replPrompt :: Repl ()
replPrompt = do
  replPutStrLn ""
  replPutStr "EnvSTLC> "
  liftIO (hFlush stdout)

replLine :: Repl ()
replLine = do
  replPrompt
  line <- liftIO $ TIO.getLine `catchIOError` handler
  let parsed = P.parse (P.only P.replItem) "stdin" line
  case parsed of
    Left err -> do
      replPutStrLn $ P.parseErrorPretty err
    Right (ReplTerm term) -> do
      replPutStr "parsed: "
      replPrint term
      do
        term' <- replTypecheckTerm term
        v <- replEvalTerm term'
        replPutStr "value: " 
        replPrint v
        replPutStr "updated term env: " 
        replPrint =<< termEnv <$> get 
      `catchError` \e -> do
        replPrint e
    Right (ReplStmt stmt) -> do
      replPutStr "parsed: "
      replPrint stmt
      do
        stmt' <- replTypecheckStmt stmt
        replPutStr "updated type env: " 
        replPrint =<< typeEnv <$> get
        replExecStmt stmt'
        replPutStr "updated term env: " 
        replPrint =<< termEnv <$> get
      `catchError` \e -> do
        replPrint e
    Right (ReplCmd c) -> case lookup c replCmds of
      Just cmd -> cmd
      Nothing -> do
        replPutStrLn $ "unknown repl command: ?" ++ T.unpack c
  where
    handler e = if isEOFError e
      then exitSuccess
      else ioError e

replLoop :: Repl ()
replLoop = replLine >> replLoop

replGarbageCollect :: Repl ()
replGarbageCollect = do
  env <- termEnv <$> get
  s <- termScope <$> get
  let (s', env') = compact s env
  updateTermScope s'
  updateTermEnv env'

replCmds :: [(T.Text, Repl())]
replCmds = [ ("gc", gc)
           , ("quit", quit)
           , ("q", quit)
           ] where
  gc = do
    replGarbageCollect
    replPutStr "updated term env: "
    replPrint =<< termEnv <$> get
  quit = replPutStrLn "" >> liftIO exitSuccess
