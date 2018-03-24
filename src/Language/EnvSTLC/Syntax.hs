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

instance Show Type where
  show IntTy = "Int"
  show BoolTy = "Bool"
  show (a :->: b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

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

instance Show (Term s) where
  show (Var x) = T.unpack x
  show (Lam x ty t) = "\\" ++ T.unpack x ++ ":" ++ show ty ++ ". " ++ show t
  show (App t1 t2) = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"
  show (IntLit n) = show n
  show (Add t1 t2) = "(" ++ show t1 ++ ") + (" ++ show t2 ++ ")"
  show (Sub t1 t2) = "(" ++ show t1 ++ ") - (" ++ show t2 ++ ")"
  show (Mul t1 t2) = "(" ++ show t1 ++ ") * (" ++ show t2 ++ ")"
  show (Div t1 t2) = "(" ++ show t1 ++ ") / (" ++ show t2 ++ ")"
  show (BoolLit b) = show b
  show (Not t) = "not (" ++ show t ++ ")"
  show (And t1 t2) = "(" ++ show t1 ++ ") && (" ++ show t2 ++ ")"
  show (Or t1 t2) = "(" ++ show t1 ++ ") || (" ++ show t2 ++ ")"
  show (IfThenElse t1 t2 t3) =
    "if (" ++ show t1 ++ ") then (" ++ show t2 ++ ") else (" ++ show t3 ++ ")"
