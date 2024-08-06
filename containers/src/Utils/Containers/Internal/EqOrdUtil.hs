module Utils.Containers.Internal.EqOrdUtil
  ( EqM(..)
  , OrdM(..)
  ) where

import Utils.Containers.Internal.StrictPair

newtype EqM a = EqM { runEqM :: a -> StrictPair Bool a }

-- | Composes left-to-right, short-circuits on False
instance Semigroup (EqM a) where
  f <> g = EqM $ \x -> case runEqM f x of
    r@(e :*: x') -> if e then runEqM g x' else r

instance Monoid (EqM a) where
  mempty = EqM (True :*:)

newtype OrdM a = OrdM { runOrdM :: a -> StrictPair Ordering a }

-- | Composes left-to-right, short-circuits on non-EQ
instance Semigroup (OrdM a) where
  f <> g = OrdM $ \x -> case runOrdM f x of
    r@(o :*: x') -> case o of
      EQ -> runOrdM g x'
      _ -> r

instance Monoid (OrdM a) where
  mempty = OrdM (EQ :*:)
