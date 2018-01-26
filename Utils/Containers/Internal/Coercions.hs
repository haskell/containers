{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

#include "containers.h"

module Utils.Containers.Internal.Coercions where

#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce
#endif

infixl 8 .#
infixr 9 #.
#if __GLASGOW_HASKELL__ >= 708
(.#) :: Coercible b a => (b -> c) -> (a -> b) -> a -> c
(.#) f _ = coerce f

(#.) :: Coercible c b => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b
#else
(.#) :: (b -> c) -> (a -> b) -> a -> c
(.#) = (.)

(#.) :: (b -> c) -> (a -> b) -> a -> c
(#.) = (.)
#endif
{-# INLINE (.#) #-}
{-# INLINE (#.) #-}

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
#if __GLASGOW_HASKELL__ >= 708
(.^#) :: Coercible c b => (a -> c -> d) -> (b -> c) -> (a -> b -> d)
(.^#) f _ = coerce f
#else
(.^#) :: (a -> c -> d) -> (b -> c) -> (a -> b -> d)
(f .^# g) x y = f x (g y)
#endif
{-# INLINE (.^#) #-}
