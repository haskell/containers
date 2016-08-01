{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE MagicHash #-}
#endif

module Data.Utils.PtrEquality (ptrEq) where

#ifdef __GLASGOW_HASKELL__
import GHC.Exts ( reallyUnsafePtrEquality# )

-- | Checks if two pointers are equal. Yes means yes;
-- no means maybe. The values should be forced to at least
-- WHNF before comparison to get moderately reliable results.
ptrEq :: a -> a -> Bool
ptrEq x y = case reallyUnsafePtrEquality# x y of
              1# -> True
              _ -> False

#else
ptrEq :: a -> a -> Bool
ptrEq _ _ = False
#endif

{-# INLINE ptrEq #-}

infix 4 `ptrEq`
