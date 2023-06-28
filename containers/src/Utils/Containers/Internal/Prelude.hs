-- | This hideous module lets us avoid dealing with the fact that
-- @liftA2@ and @foldl'@ were not previously exported from the standard prelude.
module Utils.Containers.Internal.Prelude
  ( module Prelude
  , Applicative (..)
  , Foldable (..)
  )
  where

import Prelude hiding (Applicative(..), Foldable(..))
import Control.Applicative(Applicative(..))
import Data.Foldable (Foldable(elem, foldMap, foldr, foldl, foldl', foldr1, foldl1, maximum, minimum, product, sum, null, length))
