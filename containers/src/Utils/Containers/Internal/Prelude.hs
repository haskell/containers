{-# LANGUAGE CPP #-}
-- | This hideous module lets us avoid dealing with the fact that
-- @liftA2@ wasn't previously exported from the standard prelude.
module Utils.Containers.Internal.Prelude
  ( module Prelude
  , Applicative (..)
#if !MIN_VERSION_base(4,10,0)
  , liftA2
#endif
  )
  where

import Prelude hiding (Applicative(..))
import Control.Applicative(Applicative(..))

#if !MIN_VERSION_base(4,10,0)
import Control.Applicative(liftA2)
#endif
