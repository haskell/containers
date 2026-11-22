{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
#endif

-- | Unboxed Maybe type, for internal use.
module Utils.Containers.Internal.UnboxedMaybe
  ( UMaybe
  , nothing
  , just
  , toMaybe
  ) where

#if __GLASGOW_HASKELL__ >= 810
newtype UMaybe a = UMaybe (# (# #) | a #)

-- Takes an argument because
-- Top-level bindings for unlifted types aren't allowed.
nothing :: b -> UMaybe a
nothing _ = UMaybe (# (# #) | #)

just :: a -> UMaybe a
just x = UMaybe (# | x #)

toMaybe :: UMaybe a -> Maybe a
toMaybe (UMaybe m) = case m of
  (# _ | #) -> Nothing
  (# | x #) -> Just x
{-# INLINE toMaybe #-}
#else
type UMaybe = Maybe

nothing :: b -> UMaybe a
nothing _ = Nothing

just :: a -> UMaybe a
just = Just

toMaybe :: UMaybe a -> Maybe a
toMaybe m = m
#endif
