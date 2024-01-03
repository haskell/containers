-- | 'SoloU' is an unboxed version of @Data.Tuple.Solo@.
-- For the most part, you can just pretend that we have
--
-- @data SoloU a = SoloU a@
--
-- but things are weird in older GHC versions.
{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PatternSynonyms #-}
#  if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE UnliftedNewtypes #-}
#  endif
#endif
module Utils.Containers.Internal.UnboxedSolo
  ( SoloU
  , pattern SoloU
  ) where

#ifdef __GLASGOW_HASKELL__
#  if __GLASGOW_HASKELL__ >= 810

newtype SoloU a = SoloU__ (# a #)

pattern SoloU :: a -> SoloU a
pattern SoloU a = SoloU__ (# a #)
#    if __GLASGOW_HASKELL__ >= 902
{-# INLINE SoloU #-}
#    endif

#  else

-- We have no unlifted newtypes
type SoloU a = (# a #)

pattern SoloU :: a -> SoloU a
pattern SoloU a = (# a #)

#  endif

{-# COMPLETE SoloU #-}

#else
-- Not GHC. This might be kind of slow.

data SoloU a = SoloU a

#endif
