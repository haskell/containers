{-# LANGUAGE CPP #-}
#if !defined(TESTING) && __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif

#include "containers.h"

module Data.Utils.StrictPair (StrictPair(..), toPair) where

-- | Same as regular Haskell pairs, but (x :*: _|_) = (_|_ :*: y) =
-- _|_
data StrictPair a b = !a :*: !b

toPair :: StrictPair a b -> (a, b)
toPair (x :*: y) = (x, y)
{-# INLINE toPair #-}
