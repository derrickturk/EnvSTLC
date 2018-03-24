{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.EnvSTLC.Eval (
    Value(..)
  , eval
) where

import Language.EnvSTLC.Syntax
import Language.EnvSTLC.Environment
import Control.Monad.State.Strict
import Data.Maybe (fromJust)
import qualified Data.Text as T

-- TODO: allow neutral terms?
data Value :: * where
  IntV :: Int -> Value
  BoolV :: Bool -> Value
  LamV :: Ident -> Closure (Term 'Checked) -> Value

instance Show Value where
  show (IntV n) = show n
  show (BoolV b) = show b
  show (LamV x (Closure _ t)) = "(\\" ++ T.unpack x ++ ". " ++ show t ++ ")"

type TermClosureEnv = Env (Closure (Term 'Checked))

eval :: Term 'Checked -> Value
eval t = evalState (eval' $ emptyC t) emptyE where
  eval' :: MonadState TermClosureEnv m => Closure (Term 'Checked) -> m Value
  eval' (Closure s (Var x)) = get >>= \e -> eval' (fromJust $ lookupSE x s e)
  eval' (Closure s (Lam x _ t)) = return $ LamV x (Closure s t)

  eval' (Closure s (App t1 t2)) = do
    v1 <- eval' (Closure s t1)
    case v1 of
      LamV x (Closure s' t1') -> do
        t2InEnv <- extendEnvM (Closure s t2)
        eval' (Closure ((x, t2InEnv):s') t1')
      _ -> error "uncaught application of non-lambda"

  eval' (Closure _ (IntLit n)) = return $ IntV n

  eval' (Closure s (Add t1 t2)) = evalIntOp s t1 t2 (+)
  eval' (Closure s (Sub t1 t2)) = evalIntOp s t1 t2 (-)
  eval' (Closure s (Mul t1 t2)) = evalIntOp s t1 t2 (*)
  eval' (Closure s (Div t1 t2)) = evalIntOp s t1 t2 div

  eval' (Closure _ (BoolLit b)) = return $ BoolV b

  eval' (Closure s (Not t)) = do
    v <- eval' (Closure s t)
    case v of
      BoolV b -> return $ BoolV (not b)
      _ -> error "uncaught not of non-boolean"

  eval' (Closure s (And t1 t2)) = do
    v1 <- eval' (Closure s t1)
    case v1 of
      BoolV b1 -> if b1
        then eval' (Closure s t2)
        else return $ BoolV False
      _ -> error "uncaught and on non-boolean"

  eval' (Closure s (Or t1 t2)) = do
    v1 <- eval' (Closure s t1)
    case v1 of
      BoolV b1 -> if b1
        then return $ BoolV True
        else eval' (Closure s t2)
      _ -> error "uncaught or on non-boolean"

  eval' (Closure s (IfThenElse t1 t2 t3)) = do
    v1 <- eval' (Closure s t1)
    case v1 of
      BoolV b1 -> if b1
        then eval' (Closure s t2)
        else eval' (Closure s t3)
      _ -> error "uncaught if-then-else on non-boolean"

  evalIntOp :: MonadState TermClosureEnv m
            => Scope
            -> Term 'Checked
            -> Term 'Checked
            -> (Int -> Int -> Int)
            -> m Value
  evalIntOp s t1 t2 op = do
    v1 <- eval' (Closure s t1)
    v2 <- eval' (Closure s t2)
    case (v1, v2) of
      (IntV n1, IntV n2) -> return $ IntV (n1 `op` n2)
      _ -> error "uncaught numeric operation on non-integer"
