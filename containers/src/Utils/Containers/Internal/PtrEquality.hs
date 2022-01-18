{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE MagicHash #-}
#endif

{-# OPTIONS_HADDOCK hide #-}

-- | Really unsafe pointer equality
module Utils.Containers.Internal.PtrEquality (ptrEq, hetPtrEq) where

#ifdef __GLASGOW_HASKELL__
import GHC.Exts ( reallyUnsafePtrEquality# )
import Unsafe.Coerce ( unsafeCoerce )
import GHC.Exts ( Int#, isTrue# )
#endif

-- | Checks if two pointers are equal. Yes means yes;
-- no means maybe. The values should be forced to at least
-- WHNF before comparison to get moderately reliable results.
ptrEq :: a -> a -> Bool

-- | Checks if two pointers are equal, without requiring
-- them to have the same type. The values should be forced
-- to at least WHNF before comparison to get moderately
-- reliable results.
hetPtrEq :: a -> b -> Bool

#ifdef __GLASGOW_HASKELL__
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y)
hetPtrEq x y = isTrue# (unsafeCoerce (reallyUnsafePtrEquality# :: x -> x -> Int#) x y)

#else
-- Not GHC
ptrEq _ _ = False
hetPtrEq _ _ = False
#endif

{-# INLINE ptrEq #-}
{-# INLINE hetPtrEq #-}

infix 4 `ptrEq`
infix 4 `hetPtrEq`
