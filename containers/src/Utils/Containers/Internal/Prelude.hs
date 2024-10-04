{-# LANGUAGE CPP #-}
-- | This hideous module lets us avoid dealing with the fact that
-- @liftA2@ and @foldl'@ were not previously exported from the standard prelude.
module Utils.Containers.Internal.Prelude
  ( module Prelude
  , Applicative (..)
  , Foldable (..)
#ifdef __MHS__
  , Traversable(..)
  , any, concatMap
#endif
  )
  where

#ifdef __MHS__
import Prelude hiding (elem, foldr, foldl, foldr1, foldl1, maximum, minimum, product, sum, null, length, mapM, any, concatMap)
import Data.Traversable
import Data.List.NonEmpty(NonEmpty)
import Data.Foldable(any, concatMap)
#else
import Prelude hiding (Applicative(..), Foldable(..))
#endif
import Control.Applicative(Applicative(..))
import Data.Foldable (Foldable(elem, foldMap, foldr, foldl, foldl', foldr1, foldl1, maximum, minimum, product, sum, null, length))
