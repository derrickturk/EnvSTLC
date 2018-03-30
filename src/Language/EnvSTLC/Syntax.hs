{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}

module Language.EnvSTLC.Syntax (
    Ident
  , Type(..)
  , TermState(..)
  , Stmt(..)
  , Program
  , Term(..)
  , ReplItem(..)
) where

import qualified Data.Text as T
import Data.List (intercalate)

type Ident = T.Text

data Type :: * where
  IntTy :: Type
  BoolTy :: Type
  (:->:) :: Type -> Type -> Type
  Any :: Type -- only for use in type errors (I don't love this)
infixr 0 :->: 

instance Show Type where
  show IntTy = "Int"
  show BoolTy = "Bool"
  show (a :->: b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show Any = "Any"

instance Eq Type where
  IntTy == IntTy = True
  BoolTy == BoolTy = True
  (a :->: b) == (a' :->: b') = a == a' && b == b'
  Any == _ = True
  _ == Any = True
  _ == _ = False

data TermState :: * where
  Unchecked :: TermState
  Checked :: TermState

data Stmt :: TermState -> * where
  Declare :: Ident -> Type -> Stmt s
  Define :: Ident -> Term s -> Stmt s

type Program s = [Stmt s]

instance Show (Stmt s) where
  show (Declare x ty) = T.unpack x ++ " : " ++ show ty
  show (Define x t) = T.unpack x ++ " = " ++ show t

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
  Eq :: Term s -> Term s -> Term s
  Lt :: Term s -> Term s -> Term s
  Gt :: Term s -> Term s -> Term s
  LtEq :: Term s -> Term s -> Term s
  GtEq :: Term s -> Term s -> Term s
  Let :: [Stmt s] -> Term s -> Term s
  Fix :: Term s -> Term s

instance Show (Term s) where
  show (Var x) = T.unpack x
  show (Lam x ty t) = "\\" ++ T.unpack x ++ ":" ++ show ty ++ ". " ++ show t
  show (App t1 t2) = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"
  show (IntLit n) = show n
  show (Add t1 t2) = "(" ++ show t1 ++ ") + (" ++ show t2 ++ ")"
  show (Sub t1 t2) = "(" ++ show t1 ++ ") - (" ++ show t2 ++ ")"
  show (Mul t1 t2) = "(" ++ show t1 ++ ") * (" ++ show t2 ++ ")"
  show (Div t1 t2) = "(" ++ show t1 ++ ") / (" ++ show t2 ++ ")"
  show (BoolLit True) = "true"
  show (BoolLit False) = "false"
  show (Not t) = "not (" ++ show t ++ ")"
  show (And t1 t2) = "(" ++ show t1 ++ ") && (" ++ show t2 ++ ")"
  show (Or t1 t2) = "(" ++ show t1 ++ ") || (" ++ show t2 ++ ")"
  show (IfThenElse t1 t2 t3) =
    "if (" ++ show t1 ++ ") then (" ++ show t2 ++ ") else (" ++ show t3 ++ ")"
  show (Eq t1 t2) = "(" ++ show t1 ++ ") == (" ++ show t2 ++ ")"
  show (Lt t1 t2) = "(" ++ show t1 ++ ") < (" ++ show t2 ++ ")"
  show (Gt t1 t2) = "(" ++ show t1 ++ ") > (" ++ show t2 ++ ")"
  show (LtEq t1 t2) = "(" ++ show t1 ++ ") <= (" ++ show t2 ++ ")"
  show (GtEq t1 t2) = "(" ++ show t1 ++ ") >= (" ++ show t2 ++ ")"
  show (Let stmts t) =
    "let " ++ intercalate "; " (show <$> stmts) ++ " in " ++ show t
  show (Fix t) = "fix (" ++ show t ++ ")"

data ReplItem :: * where
  ReplStmt :: Stmt 'Unchecked -> ReplItem
  ReplTerm :: Term 'Unchecked -> ReplItem
  ReplCmd :: T.Text -> ReplItem

instance Show ReplItem where
  show (ReplStmt s) = show s
  show (ReplTerm t) = show t
  show (ReplCmd c) = ':':(T.unpack c)
