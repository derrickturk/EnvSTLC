{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.EnvSTLC.Typecheck (
    Type(..)
  , TypeError(..)
  , typeOf
  , typeOfM
  , typecheck
  , typecheckM
  , typecheckStmtM
  , TypeEnv
) where

import Language.EnvSTLC.Syntax
import Language.EnvSTLC.Environment
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Coerce (coerce)
import qualified Data.Text as T

data TypeError :: * where
  TypeMismatch :: Type -> Type -> TypeError
  EqFn :: Type -> TypeError
  Undefined :: Ident -> TypeError
  MultipleDeclarations :: Ident -> TypeError
  MultipleDefinitions :: Ident -> TypeError

instance Show TypeError where
  show (TypeMismatch expected found) =
    "type error: expected " ++ show expected ++ ", found " ++ show found
  show (EqFn ty) = "type error: equality check on function type " ++ show ty
  show (Undefined x) = "type error: undefined variable \"" ++ T.unpack x ++ "\""
  show (MultipleDeclarations x) =
    "type error: multiple declarations for \"" ++ T.unpack x ++ "\""
  show (MultipleDefinitions x) =
    "type error: multiple definitions for \"" ++ T.unpack x ++ "\""

                 --  declared?  defined? (top-level bindings only)
type TypeEnv = Env (Type, Bool, Bool)

typeOf :: Term s -> Either TypeError Type
typeOf t = evalState (runExceptT $ typeOfM $ emptyC t) emptyE

typeOfM :: (MonadState TypeEnv m, MonadError TypeError m)
        => (Closure (Term s))
        -> m Type

typeOfM (Closure s (Var x)) = do
  env <- get
  let ty = lookupSE x s env
  case ty of
    -- toplevel declaration without a definition
    -- Just (_, True, False) -> throwError (Undefined x)
    Just (ty', _, _) -> return ty'
    Nothing -> throwError (Undefined x)

typeOfM (Closure s (Lam x ty t)) = do
  xInEnv <- extendEnvM (ty, False, False)
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
typeOfM (Closure s (Add t1 t2)) = typeBinOp IntTy IntTy s t1 t2
typeOfM (Closure s (Sub t1 t2)) = typeBinOp IntTy IntTy s t1 t2
typeOfM (Closure s (Mul t1 t2)) = typeBinOp IntTy IntTy s t1 t2
typeOfM (Closure s (Div t1 t2)) = typeBinOp IntTy IntTy s t1 t2

typeOfM (Closure _ (BoolLit _)) = return BoolTy
typeOfM (Closure s (And t1 t2)) = typeBinOp BoolTy BoolTy s t1 t2
typeOfM (Closure s (Or t1 t2)) = typeBinOp BoolTy BoolTy s t1 t2

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

-- it's magically polymorphic!
typeOfM (Closure s (Eq t1 t2)) = do
  t1Ty <- typeOfM (Closure s t1)
  t2Ty <- typeOfM (Closure s t2)
  case (t1Ty, t2Ty) of
    (BoolTy, BoolTy) -> return BoolTy
    (IntTy, IntTy) -> return BoolTy
    (fnTy@(_ :->: _), _) -> throwError (EqFn fnTy)
    (_, fnTy@(_ :->: _)) -> throwError (EqFn fnTy)
    (t1Ty, t2Ty) -> throwError (TypeMismatch t1Ty t2Ty)

typeOfM (Closure s (Lt t1 t2)) = typeBinOp IntTy BoolTy s t1 t2
typeOfM (Closure s (Gt t1 t2)) = typeBinOp IntTy BoolTy s t1 t2
typeOfM (Closure s (LtEq t1 t2)) = typeBinOp IntTy BoolTy s t1 t2
typeOfM (Closure s (GtEq t1 t2)) = typeBinOp IntTy BoolTy s t1 t2

typeOfM (Closure s (Let stmts t)) = do
  s' <- go s stmts
  typeOfM (Closure s' t)
  where
    go s [] = return s
    go s ((Declare x ty):rest) = extendEnvM (ty, False, False) >>=
      \i -> go ((x, i):s) rest
    go s ((Define x u):rest) = do
      env <- get
      uTy <- typeOfM (Closure s u)
      let xDeclTy = lookupSE x s env
      case xDeclTy of
        -- only check against non-toplevel declarations
        Just (xDeclTy', False, False) -> if xDeclTy' == uTy
          then go s rest
          else throwError (TypeMismatch xDeclTy' uTy)
        _ -> extendEnvM (uTy, False, False) >>= \i -> go ((x, i):s) rest

typeOfM (Closure s (Fix t)) = do
  tTy <- typeOfM (Closure s t)
  case tTy of
    fnTy@(a :->: b) -> if a == b
      then return a
      else throwError (TypeMismatch (a :->: a) fnTy)
    _ -> throwError (TypeMismatch tTy (Any :->: Any))

typeBinOp :: (MonadState TypeEnv m, MonadError TypeError m)
          => Type -> Type -> Scope -> Term s -> Term s -> m Type
typeBinOp fromTy toTy s t1 t2 = do
  t1Ty <- typeOfM (Closure s t1)
  t2Ty <- typeOfM (Closure s t2)
  if t1Ty == fromTy
    then if t2Ty == fromTy
      then return toTy
      else throwError (TypeMismatch fromTy t2Ty)
    else throwError (TypeMismatch fromTy t1Ty)

typecheck :: Term 'Unchecked -> Either TypeError (Term 'Checked)
typecheck t = evalState (runExceptT $ typecheckM $ emptyC t) emptyE

-- typeOfM will fail for type errors, otherwise coerce the term
typecheckM :: (MonadState TypeEnv m, MonadError TypeError m)
           => (Closure (Term 'Unchecked))
           -> m (Term 'Checked)
typecheckM c@(Closure _ t) = typeOfM c >> return (coerce t)

-- typecheck a top-level statement, with some current context,
--   in a stateful type environment, and produce a checked statement
--   and a new context/scope for subsequent statements
typecheckStmtM :: (MonadState TypeEnv m, MonadError TypeError m)
               => (Closure (Stmt 'Unchecked))
               -> m (Scope, Stmt 'Checked)

typecheckStmtM (Closure s stmt@(Declare x ty)) = do
  env <- get
  case lookupS x s of
    Just i -> case lookupE i env of
      Just (_, True, _) -> throwError $ MultipleDeclarations x
      Just (ty', False, True) -> if ty == ty'
        then updateEnvM i (ty, True, True) >> return (s, coerce stmt)
        else throwError (TypeMismatch ty' ty)
      _ -> addDecl s x ty
    Nothing -> addDecl s x ty
  where
    addDecl s x ty = extendEnvM (ty, True, False) >>=
      \i -> return (((x, i):s), coerce stmt)

typecheckStmtM (Closure s stmt@(Define x t)) = do
  env <- get
  ty <- typeOfM (Closure s t)
  case lookupS x s of
    Just i -> case lookupE i env of
      Just (_, _, True) -> throwError $ MultipleDefinitions x
      Just (ty', True, False) -> if ty == ty'
        then updateEnvM i (ty, True, True) >> return (s, coerce stmt)
        else throwError (TypeMismatch ty' ty)
      _ -> addDefn s x ty
    Nothing -> addDefn s x ty
  where
    addDefn s x ty = extendEnvM (ty, False, True) >>=
      \i -> return (((x, i):s), coerce stmt)
