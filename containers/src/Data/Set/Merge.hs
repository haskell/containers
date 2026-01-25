{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE Safe #-}
#endif

-- This module defines an API for writing functions that merge two sets. The key
-- functions are 'merge' and 'mergeA'. Each of these can be used with several
-- different \"merge tactics\".
--
-- @since FIXME
module Data.Set.Merge
  (
    -- ** Simple merge tactic types
    SimpleWhenMissing
  , SimpleWhenMatched

    -- ** General combining function
  , merge

    -- *** @WhenMissing@ tactics
  , dropMissing
  , preserveMissing
  , filterMissing

    -- *** @WhenMatched@ tactics
  , filterMatched

    -- ** Applicative merge tactic types
  , WhenMissing
  , WhenMatched

    -- ** Applicative general combining function
  , mergeA

    -- *** @WhenMissing@ tactics
    -- | The tactics described for 'merge' work for 'mergeA' as well.
    -- Furthermore, the following are available.
  , filterAMissing

    -- *** @WhenMatched@ tactics
    -- | The tactics described for 'merge' work for 'mergeA' as well.
    -- Furthermore, the following are available.
  , filterAMatched

    -- ** Miscellaneous tactic functions
  , runWhenMissing
  , runWhenMatched
  ) where

import Data.Set.Internal
