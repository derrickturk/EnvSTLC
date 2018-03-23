{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}

module Language.EnvSTLC.Environment (
    Scope
  , Closure(..)
  , Env(..)
  , lookupC
  , emptyS
  , lookupS
  , emptyC
  , lookupE
  , emptyE
  , lookupSE
  , lookupCE
) where

import Language.EnvSTLC.Syntax
import Prelude hiding (lookup)
import qualified Prelude as P
import qualified Data.Sequence as S

type Scope = [(Ident, Int)]

data Closure :: * -> * where
  Closure :: Scope -> a -> Closure a

data Env :: * where
  Env :: S.Seq (Closure (Term 'Checked)) -> Env

lookupS :: Ident -> Scope -> Maybe Int
lookupS = P.lookup

emptyS :: Scope
emptyS = []

lookupC :: Ident -> Closure a -> Maybe Int
lookupC x (Closure s _) = lookupS x s

emptyC :: a -> Closure a
emptyC = Closure []

lookupE :: Int -> Env -> Maybe (Closure (Term 'Checked))
lookupE i (Env e) = S.lookup i e

emptyE :: Env
emptyE = Env S.empty

lookupSE :: Ident
         -> Scope
         -> Env
         -> Maybe (Closure (Term 'Checked))
lookupSE x s e = lookupS x s >>= flip lookupE e

lookupCE :: Ident
         -> Closure (Term 'Checked)
         -> Env
         -> Maybe (Closure (Term 'Checked))
lookupCE x c e = lookupC x c >>= flip lookupE e
