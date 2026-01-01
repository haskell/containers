{-# LANGUAGE CPP #-}
module Utils.Containers.Internal.EqOrdUtil
  ( EqM(..)
  , OrdM(..)
  ) where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif
import Utils.Containers.Internal.Strict (StrictPair(..))

newtype EqM a = EqM { runEqM :: a -> StrictPair Bool a }

-- | Composes left-to-right, short-circuits on False
instance Semigroup (EqM a) where
  f <> g = EqM $ \x -> case runEqM f x of
    r@(e :*: x') -> if e then runEqM g x' else r

instance Monoid (EqM a) where
  mempty = EqM (True :*:)
#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif

newtype OrdM a = OrdM { runOrdM :: a -> StrictPair Ordering a }

-- | Composes left-to-right, short-circuits on non-EQ
instance Semigroup (OrdM a) where
  f <> g = OrdM $ \x -> case runOrdM f x of
    r@(o :*: x') -> case o of
      EQ -> runOrdM g x'
      _ -> r

instance Monoid (OrdM a) where
  mempty = OrdM (EQ :*:)
#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif
