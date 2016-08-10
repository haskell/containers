{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE MagicHash #-}
#endif

module Data.Utils.PtrEquality (ptrEq) where

#ifdef __GLASGOW_HASKELL__
import GHC.Exts ( reallyUnsafePtrEquality# )
#if __GLASGOW_HASKELL__ < 707
import GHC.Exts ( (==#) )
#else
import GHC.Exts ( isTrue# )
#endif
#endif

-- | Checks if two pointers are equal. Yes means yes;
-- no means maybe. The values should be forced to at least
-- WHNF before comparison to get moderately reliable results.
ptrEq :: a -> a -> Bool

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 707
ptrEq x y = reallyUnsafePtrEquality# x y ==# 1#
#else
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y)
#endif

#else
-- Not GHC
ptrEq _ _ = False
#endif

{-# INLINE ptrEq #-}

infix 4 `ptrEq`
