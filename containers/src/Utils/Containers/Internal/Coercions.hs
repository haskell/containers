{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

#include "containers.h"

module Utils.Containers.Internal.Coercions where

#ifdef __GLASGOW_HASKELL__
import Data.Coerce
#endif

infixl 8 .#
#ifdef __GLASGOW_HASKELL__
(.#) :: Coercible b a => (b -> c) -> (a -> b) -> a -> c
(.#) f _ = coerce f
#else
(.#) :: (b -> c) -> (a -> b) -> a -> c
(.#) = (.)
#endif
{-# INLINE (.#) #-}

infix 9 .^#

-- | Coerce the second argument of a function. Conceptually,
-- can be thought of as:
--
-- @
--   (f .^# g) x y = f x (g y)
-- @
--
-- However it is most useful when coercing the arguments to
-- 'foldl':
--
-- @
--   foldl f b . fmap g = foldl (f .^# g) b
-- @
#ifdef __GLASGOW_HASKELL__
(.^#) :: Coercible c b => (a -> c -> d) -> (b -> c) -> (a -> b -> d)
(.^#) f _ = coerce f
#else
(.^#) :: (a -> c -> d) -> (b -> c) -> (a -> b -> d)
(f .^# g) x y = f x (g y)
#endif
{-# INLINE (.^#) #-}
