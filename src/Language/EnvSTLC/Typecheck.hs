{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}

module Language.EnvSTLC.Typecheck (
    Type(..)
  , TypeError(..)
  , typeOf
  , typecheck
) where

import Language.EnvSTLC.Syntax

data TypeError = TypeError { expected :: Type
                           , found :: Type
                           }

typeOf :: Term s -> Either TypeError Type
typeOf = undefined

typecheck :: Term 'Unchecked -> Either TypeError (Term 'Checked)
typecheck = undefined
