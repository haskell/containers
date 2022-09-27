{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE MagicHash #-}
#endif

module Utils.IsUnit (isUnit, isUnitSupported) where

#ifdef __GLASGOW_HASKELL__
import GHC.Exts
#endif

-- | Check whether the argument is a fully evaluated unit `()`.
--
-- Always returns `False` is `isUnitSupported` returns `False`.
--
-- Uses `reallyUnsafePtrEquality#`.
isUnit :: () -> Bool

-- | Checks whether `isUnit` is supported by the Haskell implementation.
--
-- Currently returns `True` for ghc and `False` for all other implementations.
isUnitSupported :: Bool

#ifdef __GLASGOW_HASKELL__

-- simplified from  Utils.Containers.Internal.PtrEquality
ptrEq :: a -> a -> Bool
ptrEq x y = case reallyUnsafePtrEquality# x y of
    0# -> False
    _  -> True

isUnit = ptrEq ()

isUnitSupported = True

#else /* !__GLASGOW_HASKELL__ */

isUnit = False

isUnitSupported = False

#endif
