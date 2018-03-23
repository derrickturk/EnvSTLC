{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}

module Language.EnvSTLC.Syntax (
    Ident
  , Type(..)
  , TermState(..)
  , Term(..)
) where

import Data.Text as T

type Ident = T.Text

data Type :: * where
  IntTy :: Type
  BoolTy :: Type
  (:->:) :: Type -> Type -> Type
infixr 0 :->: 

data TermState :: * where
  Unchecked :: TermState
  Checked :: TermState

data Term :: TermState -> * where
  Var :: Ident -> Term s
  Lam :: Ident -> Type -> Term s -> Term s
  App :: Term s -> Term s -> Term s
  IntLit :: Int -> Term s
  Add :: Term s -> Term s -> Term s
  Sub :: Term s -> Term s -> Term s
  Mul :: Term s -> Term s -> Term s
  Div :: Term s -> Term s -> Term s
  BoolLit :: Bool -> Term s 
  Not :: Term s -> Term s
  And :: Term s -> Term s -> Term s
  Or :: Term s -> Term s -> Term s
  IfThenElse :: Term s -> Term s -> Term s -> Term s
