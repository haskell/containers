{-# LANGUAGE CPP #-}

#include "containers.h"

module Utils.Containers.Internal.Coercions where

#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce
#endif

infixr 9 .#, #.
#if __GLASGOW_HASKELL__ >= 708
(.#) :: Coercible b a => (b -> c) -> (a -> b) -> a -> c
(.#) f _ = coerce f
(#.) :: Coercible c b => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce
#else
(.#) :: (b -> c) -> (a -> b) -> a -> c
(.#) = (.)
(#.) :: (b -> c) -> (a -> b) -> a -> c
(#.) = (.)
#endif
{-# INLINE (.#) #-}
{-# INLINE (#.) #-}
