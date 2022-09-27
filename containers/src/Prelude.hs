{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
-- | This hideous module lets us avoid dealing with the fact that
-- @liftA2@ wasn't previously exported from the standard prelude.
module Prelude
  ( module Prel
  , Applicative (..)
#if !MIN_VERSION_base(4,10,0)
  , liftA2
#endif
  )
  where

import "base" Prelude as Prel hiding (Applicative(..))
import Control.Applicative(Applicative(..))

#if !MIN_VERSION_base(4,10,0)
import Control.Applicative(liftA2)
#endif
