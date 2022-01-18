{-# LANGUAGE CPP #-}
#include "containers.h"
{-# OPTIONS_HADDOCK hide #-}

-- | A clone of Control.Monad.State.Strict.
module Utils.Containers.Internal.State where

import Control.Monad (ap, liftM2)
import Control.Applicative (Applicative(..), liftA)

newtype State s a = State {runState :: s -> (s, a)}

instance Functor (State s) where
    fmap = liftA

instance Monad (State s) where
    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    return = pure
    m >>= k = State $ \ s -> case runState m s of
        (s', x) -> runState (k x) s'

instance Applicative (State s) where
    {-# INLINE pure #-}
    pure x = State $ \ s -> (s, x)
    (<*>) = ap
    m *> n = State $ \s -> case runState m s of
      (s', _) -> runState n s'
#if MIN_VERSION_base(4,10,0)
    liftA2 = liftM2
#endif

execState :: State s a -> s -> a
execState m x = snd (runState m x)
