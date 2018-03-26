{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.EnvSTLC.Environment (
    Scope
  , Closure(..)
  , Env(..)
  , envLength
  , lookupC
  , emptyS
  , lookupS
  , emptyC
  , lookupE
  , emptyE
  , lookupSE
  , lookupCE
  , extendEnv
  , extendEnvM
  , updateEnvM
) where

import Language.EnvSTLC.Syntax
import Prelude hiding (lookup)
import qualified Prelude as P
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Control.Monad.State.Strict
import Data.List (intercalate)

type Scope = [(Ident, Int)]

data Closure :: * -> * where
  Closure :: Scope -> a -> Closure a

instance Show a => Show (Closure a) where
  show (Closure [] x) = show x
  show (Closure c x) = show x ++ "[with " ++ intercalate ", " binds ++ "]" where
    binds = fmap (\(x, i) -> T.unpack x ++ "@" ++ show i) c

data Env :: * -> * where
  Env :: Seq.Seq a -> Env a

instance Show a => Show (Env a) where
  show (Env e) = foldl (++) "" (Seq.intersperse ", " defs) where
    defs = Seq.mapWithIndex (\i x -> show i ++ " = " ++ show x) e

envLength :: Env a -> Int
envLength (Env e) = Seq.length e 

lookupS :: Ident -> Scope -> Maybe Int
lookupS = P.lookup

emptyS :: Scope
emptyS = []

lookupC :: Ident -> Closure a -> Maybe Int
lookupC x (Closure s _) = lookupS x s

emptyC :: a -> Closure a
emptyC = Closure []

lookupE :: Int -> Env a -> Maybe a
lookupE i (Env e) = Seq.lookup i e

emptyE :: Env a
emptyE = Env Seq.empty

lookupSE :: Ident -> Scope -> Env a -> Maybe a
lookupSE x s e = lookupS x s >>= flip lookupE e

lookupCE :: Ident -> Closure a -> Env b -> Maybe b
lookupCE x c e = lookupC x c >>= flip lookupE e

extendEnv :: Env a -> a -> (Int, Env a)
extendEnv (Env e) x = let next = Seq.length e in (next, Env $ e Seq.|> x)

-- store a term closure in the enviroment and return the index
extendEnvM :: MonadState (Env a) m => a -> m Int
extendEnvM t = do
  e <- get
  let (next, e') = extendEnv e t
  put e'
  return next

-- change the value at a specified index
updateEnvM :: MonadState (Env a) m => Int -> a -> m ()
updateEnvM i x = modify (\(Env e) -> Env $ Seq.update i x e)
