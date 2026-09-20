{-# LANGUAGE CPP #-}
module Utils.Containers.Internal.ShortCircuit
  ( ShortCircuit(..)
  ) where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif
import Utils.Containers.Internal.Strict (StrictPair(..))

-- | Composes left-to-right, short-circuits on monoid identity
newtype ShortCircuit m a = ShortCircuit { runShortCircuit :: a -> StrictPair m a }

instance (Eq m, Monoid m) => Semigroup (ShortCircuit m a) where
  f <> g = ShortCircuit $ \x -> case runShortCircuit f x of
    r@(e :*: x') -> if e == mempty then runShortCircuit g x' else r

instance (Eq m, Monoid m) => Monoid (ShortCircuit m a) where
  mempty = ShortCircuit (mempty :*:)
#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif
