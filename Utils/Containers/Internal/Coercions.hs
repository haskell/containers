{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

#include "containers.h"

module Utils.Containers.Internal.Coercions where

#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce
#endif

infixl 8 .#
#if __GLASGOW_HASKELL__ >= 708
(.#) :: Coercible b a => (b -> c) -> (a -> b) -> a -> c
(.#) f _ = coerce f
#else
(.#) :: (b -> c) -> (a -> b) -> a -> c
(.#) = (.)
#endif
{-# INLINE (.#) #-}
