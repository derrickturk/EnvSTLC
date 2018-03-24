{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}

module Language.EnvSTLC.Typecheck (
    Type(..)
  , TypeError(..)
  , typeOf
  , typecheck
) where

import Language.EnvSTLC.Syntax
import qualified Data.Text as T

data TypeError =
    TypeMismatch Type Type
  | Undefined Ident

instance Show TypeError where
  show (TypeMismatch expected found) =
    "type error: expected " ++ show expected ++ ", found " ++ show found
  show (Undefined x) = "type error: undefined variable \"" ++ T.unpack x ++ "\""

typeOf :: Term s -> Either TypeError Type
typeOf = typeOf' [] where
  typeOf' _ _ = undefined

typecheck :: Term 'Unchecked -> Either TypeError (Term 'Checked)
typecheck = undefined
