{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.EnvSTLC.Eval (
    Value(..)
  , eval
  , evalM
  , evalEnv
  , TermClosureEnv
) where

import Language.EnvSTLC.Syntax
import Language.EnvSTLC.Environment
import Control.Monad.State.Strict
import Control.Monad (foldM)
import Data.Maybe (fromJust)
import qualified Data.Text as T

-- TODO: allow neutral terms?
data Value :: * where
  IntV :: Int -> Value
  BoolV :: Bool -> Value
  LamV :: Ident -> Type -> Closure (Term 'Checked) -> Value

instance Show Value where
  show (IntV n) = show n
  show (BoolV True) = "true"
  show (BoolV False) = "false"
  show (LamV x ty t) =
    "(\\" ++ T.unpack x ++ ":" ++ show ty ++ ". " ++ show t ++ ")"

type TermClosureEnv = Env (Closure (Term 'Checked))

eval :: Term 'Checked -> Value
eval t = evalState (evalM $ emptyC t) emptyE

evalEnv :: Term 'Checked -> (Value, TermClosureEnv)
evalEnv t = runState (evalM $ emptyC t) emptyE

evalM :: MonadState TermClosureEnv m => Closure (Term 'Checked) -> m Value

evalM (Closure s (Var x)) = get >>= \e -> evalM (fromJust $ lookupSE x s e)
evalM (Closure s (Lam x ty t)) = return $ LamV x ty (Closure s t)

evalM (Closure s (App t1 t2)) = do
  v1 <- evalM (Closure s t1)
  case v1 of
    LamV x _ (Closure s' t1') -> do
      t2InEnv <- extendEnvM (Closure s t2)
      evalM (Closure ((x, t2InEnv):s') t1')
    _ -> error "uncaught application of non-lambda"

evalM (Closure _ (IntLit n)) = return $ IntV n

evalM (Closure s (Add t1 t2)) = evalIntOp s t1 t2 (+)
evalM (Closure s (Sub t1 t2)) = evalIntOp s t1 t2 (-)
evalM (Closure s (Mul t1 t2)) = evalIntOp s t1 t2 (*)
evalM (Closure s (Div t1 t2)) = evalIntOp s t1 t2 div

evalM (Closure _ (BoolLit b)) = return $ BoolV b

evalM (Closure s (Not t)) = do
  v <- evalM (Closure s t)
  case v of
    BoolV b -> return $ BoolV (not b)
    _ -> error "uncaught not of non-boolean"

evalM (Closure s (And t1 t2)) = do
  v1 <- evalM (Closure s t1)
  case v1 of
    BoolV b1 -> if b1
      then evalM (Closure s t2)
      else return $ BoolV False
    _ -> error "uncaught and on non-boolean"

evalM (Closure s (Or t1 t2)) = do
  v1 <- evalM (Closure s t1)
  case v1 of
    BoolV b1 -> if b1
      then return $ BoolV True
      else evalM (Closure s t2)
    _ -> error "uncaught or on non-boolean"

evalM (Closure s (IfThenElse t1 t2 t3)) = do
  v1 <- evalM (Closure s t1)
  case v1 of
    BoolV b1 -> if b1
      then evalM (Closure s t2)
      else evalM (Closure s t3)
    _ -> error "uncaught if-then-else on non-boolean"

evalM (Closure s (Let stmts t)) = do
  s' <- foldM (\s stmt -> execM (Closure s stmt)) s stmts
  evalM (Closure s' t)

evalIntOp :: MonadState TermClosureEnv m
          => Scope
          -> Term 'Checked
          -> Term 'Checked
          -> (Int -> Int -> Int)
          -> m Value
evalIntOp s t1 t2 op = do
  v1 <- evalM (Closure s t1)
  v2 <- evalM (Closure s t2)
  case (v1, v2) of
    (IntV n1, IntV n2) -> return $ IntV (n1 `op` n2)
    _ -> error "uncaught numeric operation on non-integer"

execM :: MonadState TermClosureEnv m => Closure (Stmt 'Checked) -> m Scope
execM (Closure s (Declare _ _)) = return s
execM (Closure s (Define x t)) = do
  xInEnv <- extendEnvM (Closure s t)
  return ((x, xInEnv):s)
