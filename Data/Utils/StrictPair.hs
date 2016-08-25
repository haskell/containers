{-# LANGUAGE CPP #-}
#if !defined(TESTING) && __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif

#include "containers.h"

{-# OPTIONS_HADDOCK hide #-}

-- | A strict pair
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- This contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.

module Data.Utils.StrictPair (StrictPair(..), toPair) where

-- | Same as regular Haskell pairs, but (x :*: _|_) = (_|_ :*: y) =
-- _|_
data StrictPair a b = !a :*: !b

toPair :: StrictPair a b -> (a, b)
toPair (x :*: y) = (x, y)
{-# INLINE toPair #-}
