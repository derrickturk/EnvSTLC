{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.EnvSTLC.Eval (
    Value(..)
  , eval
  , evalM
  , evalEnv
  , execM
  , TermClosureEnv
  , garbageCollectM
  , compact
) where

import Language.EnvSTLC.Syntax
import Language.EnvSTLC.Environment
import Control.Monad.State.Strict
import Control.Monad (foldM)
import Data.Maybe (fromJust)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Sequence as Seq

-- TODO: allow neutral terms?
data Value :: * where
  IntV :: Integer -> Value
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

evalM (Closure s (Add t1 t2)) = evalIntOp s t1 t2 (+) IntV
evalM (Closure s (Sub t1 t2)) = evalIntOp s t1 t2 (-) IntV
evalM (Closure s (Mul t1 t2)) = evalIntOp s t1 t2 (*) IntV
evalM (Closure s (Div t1 t2)) = evalIntOp s t1 t2 div IntV

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

evalM (Closure s (Eq t1 t2)) = do
  v1 <- evalM (Closure s t1)
  v2 <- evalM (Closure s t2)
  return $ BoolV $ case (v1, v2) of
    (BoolV b1, BoolV b2) -> b1 == b2
    (IntV i1, IntV i2) -> i1 == i2
    _ -> error "uncaught invalid equality test"

evalM (Closure s (Lt t1 t2)) = evalIntOp s t1 t2 (<) BoolV
evalM (Closure s (Gt t1 t2)) = evalIntOp s t1 t2 (>) BoolV
evalM (Closure s (LtEq t1 t2)) = evalIntOp s t1 t2 (<=) BoolV
evalM (Closure s (GtEq t1 t2)) = evalIntOp s t1 t2 (>=) BoolV

evalM (Closure s (Let stmts t)) = do
  s' <- foldM (\s stmt -> execM (Closure s stmt)) s stmts
  evalM (Closure s' t)

evalM (Closure s (Fix t)) = do
  v <- evalM (Closure s t)
  case v of
    (LamV x ty (Closure s' u)) ->
      evalM (Closure s' (Let [Define x (Fix (Lam x ty u))] u))
    _ -> error "uncaught fix on non-function"

evalIntOp :: MonadState TermClosureEnv m
          => Scope
          -> Term 'Checked
          -> Term 'Checked
          -> (Integer -> Integer -> a)
          -> (a -> Value)
          -> m Value
evalIntOp s t1 t2 op ctor = do
  v1 <- evalM (Closure s t1)
  v2 <- evalM (Closure s t2)
  case (v1, v2) of
    (IntV n1, IntV n2) -> return $ ctor (n1 `op` n2)
    _ -> error "uncaught integer operation on non-integer"

execM :: MonadState TermClosureEnv m => Closure (Stmt 'Checked) -> m Scope
execM (Closure s (Declare _ _)) = return s
execM (Closure s (Define x t)) = do
  xInEnv <- extendEnvM (Closure s t)
  return ((x, xInEnv):s)

-- remove all environment entries unreachable from a given scope and
--   return the scope adjusted to the new environment
garbageCollectM :: MonadState TermClosureEnv m => Scope -> m Scope
garbageCollectM s = do
  env <- get
  let (s', env') = compact s env
  put env'
  return s'

reachable :: Scope -> TermClosureEnv -> S.Set Int
reachable s env = go S.empty s env where
  go set [] _ = set
  go set ((_, i):rest) env =
    go (S.insert i set `S.union` go S.empty (closureScope i env) env) rest env

  closureScope i env = case lookupE i env of
    Just (Closure s' _) -> s'
    _ -> []

-- compact an environment e to only entries reachable from s
--   and return an updated s and e
compact :: Scope -> TermClosureEnv -> (Scope, TermClosureEnv)
compact s env = (adjustS s, adjustE env) where
  adjustS s = fmap adjustB s
  adjustB (x, i) = (x, i - S.size (S.filter (< i) drop))
  adjustE (Env e) = Env $ foldl' (flip Seq.deleteAt) e (S.toDescList drop)
  drop = S.fromList [0 .. envLength env] `S.difference` reachable s env
