{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}
#endif

module Utils.Containers.Internal.UnboxedMaybe
  (
#ifdef __GLASGOW_HASKELL__
    Maybe
  , pattern Nothing
  , pattern Just
#else
    Maybe(..)
#endif
  , toMaybe
  , mapMaybe
  ) where

#ifdef __GLASGOW_HASKELL__
import Prelude hiding (Maybe(..))
import qualified Prelude as P
#else
import Data.Maybe (mapMaybe)
#endif

#ifdef __GLASGOW_HASKELL__
-- On GHC, use unboxed sums.

type Maybe a = (# (# #) | a #)

pattern Nothing :: Maybe a
pattern Nothing = (# (# #) | #)

pattern Just :: a -> Maybe a
pattern Just x = (# | x #)

{-# COMPLETE Nothing, Just #-}

toMaybe :: Maybe a -> P.Maybe a
toMaybe Nothing = P.Nothing
toMaybe (Just x) = P.Just x

-- We don't need list fusion, for now.
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = go
  where
    go [] = []
    go (x:xs) = case f x of
      Nothing -> go xs
      Just y -> y : go xs
{-# INLINE mapMaybe #-}
#else
-- On non-GHC, use the regular Maybe.

toMaybe :: Maybe a -> Maybe a
toMaybe = id
#endif
