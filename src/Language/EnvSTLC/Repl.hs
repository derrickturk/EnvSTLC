{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Language.EnvSTLC.Repl (
    ReplItem
  , ReplState(..)
  , Repl
  , unRepl
  , updateTypeScope
  , updateTermScope
  , updateTypeEnv
  , updateTermEnv
  , replTypecheckStmt
  , replTypecheckTerm
  , replExecStmt
  , replEvalTerm
) where

import Language.EnvSTLC.Syntax
import Language.EnvSTLC.Environment
import Language.EnvSTLC.Typecheck
import Language.EnvSTLC.Eval
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.Text as T

data ReplState = ReplState { typeScope :: Scope
                           , termScope :: Scope
                           , typeEnv :: TypeEnv
                           , termEnv :: TermClosureEnv
                           } deriving Show

newtype Repl a = Repl { unRepl :: ExceptT TypeError (State ReplState) a }
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadError TypeError
    , MonadState ReplState
  )

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
