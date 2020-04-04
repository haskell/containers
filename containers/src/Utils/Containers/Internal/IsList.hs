{-# LANGUAGE CPP, Trustworthy #-}

-- | A @Trustworthy@ module for the sole purpose of exposing 'GHC.Exts.IsList'.
-- To ensure that this is not misused, this module is not and should not be
-- exposed, and we only export the safe methods ('fromList') and ('toList').
module Utils.Containers.Internal.IsList (
#if __GLASGOW_HASKELL__ >= 708
    IsList(Item, fromList, toList)
#endif
) where

#if __GLASGOW_HASKELL__ >= 708
import GHC.Exts (IsList(Item, fromList, toList))
#endif
