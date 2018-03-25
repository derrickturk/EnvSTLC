{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.EnvSTLC.Typecheck (
    Type(..)
  , TypeError(..)
  , typeOf
  , typeOfM
  , typecheck
  , typecheckM
) where

import Language.EnvSTLC.Syntax
import Language.EnvSTLC.Environment
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Coerce (coerce)
import qualified Data.Text as T

data TypeError =
    TypeMismatch Type Type
  | Undefined Ident

instance Show TypeError where
  show (TypeMismatch expected found) =
    "type error: expected " ++ show expected ++ ", found " ++ show found
  show (Undefined x) = "type error: undefined variable \"" ++ T.unpack x ++ "\""

type TypeEnv = Env Type

typeOf :: Term s -> Either TypeError Type
typeOf t = evalState (runExceptT $ typeOfM $ emptyC t) emptyE

typeOfM :: (MonadState TypeEnv m, MonadError TypeError m)
        => (Closure (Term s))
        -> m Type

typeOfM (Closure s (Var x)) = do
  env <- get
  let ty = lookupSE x s env
  case ty of
    Just ty' -> return ty'
    Nothing -> throwError (Undefined x)

typeOfM (Closure s (Lam x ty t)) = do
  xInEnv <- extendEnvM ty
  resTy <- typeOfM (Closure ((x, xInEnv):s) t)
  return $ ty :->: resTy

typeOfM (Closure s (App t1 t2)) = do
  t1Ty <- typeOfM (Closure s t1)
  t2Ty <- typeOfM (Closure s t2)
  case t1Ty of
    a :->: b -> if t2Ty == a
      then return b
      else throwError (TypeMismatch a t2Ty)
    _ -> throwError (TypeMismatch (t2Ty :->: Any) t1Ty)

typeOfM (Closure _ (IntLit _)) = return IntTy
typeOfM (Closure s (Add t1 t2)) = typeBinOp IntTy s t1 t2
typeOfM (Closure s (Sub t1 t2)) = typeBinOp IntTy s t1 t2
typeOfM (Closure s (Mul t1 t2)) = typeBinOp IntTy s t1 t2
typeOfM (Closure s (Div t1 t2)) = typeBinOp IntTy s t1 t2

typeOfM (Closure _ (BoolLit _)) = return BoolTy
typeOfM (Closure s (And t1 t2)) = typeBinOp BoolTy s t1 t2
typeOfM (Closure s (Or t1 t2)) = typeBinOp BoolTy s t1 t2

typeOfM (Closure s (Not t)) = do
  tTy <- typeOfM (Closure s t)
  if tTy == BoolTy
    then return BoolTy
    else throwError (TypeMismatch BoolTy tTy)

typeOfM (Closure s (IfThenElse t1 t2 t3)) = do
  t1Ty <- typeOfM (Closure s t1)
  t2Ty <- typeOfM (Closure s t2)
  t3Ty <- typeOfM (Closure s t3)
  if t1Ty == BoolTy
    then if t2Ty == t3Ty
      then return t2Ty
      else throwError (TypeMismatch t2Ty t3Ty)
    else throwError (TypeMismatch BoolTy t1Ty)

typeOfM (Closure s (Let stmts t)) = do
  s' <- go s stmts
  typeOfM (Closure s' t)
  where
    go s [] = return s
    go s ((Declare x ty):rest) = extendEnvM ty >>= \i -> go ((x, i):s) rest
    go s ((Define x u):rest) = do
      env <- get
      uTy <- typeOfM (Closure s u)
      let xDeclTy = lookupSE x s env
      case xDeclTy of
        Just xDeclTy' -> if xDeclTy' == uTy
          then go s rest
          else throwError (TypeMismatch xDeclTy' uTy)
        Nothing -> extendEnvM uTy >>= \i -> go ((x, i):s) rest

typeBinOp :: (MonadState TypeEnv m, MonadError TypeError m)
          => Type -> Scope -> Term s -> Term s -> m Type
typeBinOp ty s t1 t2 = do
  t1Ty <- typeOfM (Closure s t1)
  t2Ty <- typeOfM (Closure s t2)
  if t1Ty == ty
    then if t2Ty == ty
      then return ty
      else throwError (TypeMismatch ty t2Ty)
    else throwError (TypeMismatch ty t1Ty)

typecheck :: Term 'Unchecked -> Either TypeError (Term 'Checked)
typecheck t = evalState (runExceptT $ typecheckM $ emptyC t) emptyE

-- typeOfM will fail for type errors, otherwise coerce the term
typecheckM :: (MonadState TypeEnv m, MonadError TypeError m)
           => (Closure (Term 'Unchecked))
           -> m (Term 'Checked)
typecheckM c@(Closure _ t) = typeOfM c >> return (coerce t)
