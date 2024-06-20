{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE MagicHash #-}
#endif

{-# OPTIONS_HADDOCK hide #-}

-- | Really unsafe pointer equality
module Utils.Containers.Internal.PtrEquality (ptrEq) where

#ifdef __GLASGOW_HASKELL__
import GHC.Exts ( isTrue#, reallyUnsafePtrEquality# )
#endif

-- | Checks if two pointers are equal. Yes means yes;
-- no means maybe. The values should be forced to at least
-- WHNF before comparison to get moderately reliable results.
ptrEq :: a -> a -> Bool

#ifdef __GLASGOW_HASKELL__
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y)
#else
-- Not GHC
ptrEq _ _ = False
#endif

{-# INLINE ptrEq #-}

infix 4 `ptrEq`
