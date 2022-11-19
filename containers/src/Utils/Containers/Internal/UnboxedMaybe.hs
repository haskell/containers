{-# language CPP #-}
{-# language LambdaCase #-}
#if defined(__GLASGOW_HASKELL__)
{-# language PatternSynonyms #-}
#  if __GLASGOW_HASKELL__ >= 802
{-# language UnboxedSums #-}
{-# language UnboxedTuples #-}
#  endif
#  if __GLASGOW_HASKELL__ >= 810
{-# language UnliftedNewtypes #-}
#  endif
#endif
module Utils.Containers.Internal.UnboxedMaybe
  ( MaybeU
  , pattern JustU
  , pattern NothingU
  , maybeU
  , toMaybe
  , toMaybeU
  ) where

#if defined(__GLASGOW_HASKELL__)
#  if __GLASGOW_HASKELL__ >= 810
newtype MaybeU a = MaybeU (# (##) | a #)

pattern NothingU :: MaybeU a
pattern NothingU = MaybeU (# (##)| #)
#    if __GLASGOW_HASKELL__ >= 902
{-# INLINE NothingU #-}
#    endif

pattern JustU :: a -> MaybeU a
pattern JustU a = MaybeU (#|a #)
#    if __GLASGOW_HASKELL__ >= 902
{-# INLINE JustU #-}
#    endif

#  elif __GLASGOW_HASKELL__ >= 802
-- We have no unlifted newtypes. Yuck.
type MaybeU a = (# (##) | a #)

pattern NothingU :: MaybeU a
pattern NothingU = (# (##)| #)

pattern JustU :: a -> MaybeU a
pattern JustU a = (#|a #)

#  else
-- We have no unboxed sums.

newtype MaybeU a = MaybeU (Maybe a)

pattern NothingU :: MaybeU a
pattern NothingU = MaybeU Nothing

pattern JustU :: a -> MaybeU a
pattern JustU a = MaybeU (Just a)

#  endif

{-# COMPLETE JustU, NothingU #-}

#else
-- We don't even have pattern synonyms. We can hope that inlining will
-- prevent this from being a performance disaster, but if this code ever
-- gets used, someone will have to check on that.

data MaybeU a = NothingU | JustU a
#endif

maybeU :: r -> (a -> r) -> MaybeU a -> r
maybeU n j = \case
  NothingU -> n
  JustU a -> j a
{-# INLINE maybeU #-}

toMaybe :: MaybeU a -> Maybe a
toMaybe = maybeU Nothing Just
{-# INLINE toMaybe #-}

toMaybeU :: Maybe a -> MaybeU a
toMaybeU Nothing = NothingU
toMaybeU (Just a) = JustU a
{-# INLINE toMaybeU #-}
