{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE Safe #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map.Merge.Strict
-- Copyright   :  (c) David Feuer 2016
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- This module defines an API for writing functions that merge two
-- maps. The key functions are 'merge' and 'mergeA'.
-- Each of these can be used with several different \"merge tactics\".
--
-- The 'merge' and 'mergeA' functions are shared by
-- the lazy and strict modules. Only the choice of merge tactics
-- determines strictness. If you use 'Data.Map.Merge.Strict.mapMissing'
-- from this module then the results will be forced before they are
-- inserted. If you use 'Data.Map.Merge.Lazy.mapMissing' from
-- "Data.Map.Merge.Lazy" then they will not.
--
-- == 'preserveMissing' inconsistency
--
-- For historical reasons, the preserved values are //not// forced. To force
-- them, use 'preserveMissing''.
--
-- == Efficiency note
--
-- The 'Control.Category.Category', 'Applicative', and 'Monad' instances for
-- 'WhenMissing' tactics are included because they are valid. However, they are
-- inefficient in many cases and should usually be avoided. The instances
-- for 'WhenMatched' tactics should not pose any major efficiency problems.
--
-- @since 0.5.9

module Data.Map.Merge.Strict (
    -- ** Simple merge tactic types
      SimpleWhenMissing
    , SimpleWhenMatched

    -- ** General combining function
    , merge

    -- *** @WhenMatched@ tactics
    , zipWithMaybeMatched
    , zipWithMatched

    -- *** @WhenMissing@ tactics
    , mapMaybeMissing
    , dropMissing
    , preserveMissing
    , preserveMissing'
    , mapMissing
    , filterMissing

    -- ** Applicative merge tactic types
    , WhenMissing
    , WhenMatched

    -- ** Applicative general combining function
    , mergeA

    -- *** @WhenMatched@ tactics
    -- | The tactics described for 'merge' work for
    -- 'mergeA' as well. Furthermore, the following
    -- are available.
    , zipWithMaybeAMatched
    , zipWithAMatched

    -- *** @WhenMissing@ tactics
    -- | The tactics described for 'merge' work for
    -- 'mergeA' as well. Furthermore, the following
    -- are available.
    , traverseMaybeMissing
    , traverseMissing
    , filterAMissing

    -- ** Covariant maps for tactics
    , mapWhenMissing
    , mapWhenMatched

    -- ** Miscellaneous functions on tactics

    , runWhenMatched
    , runWhenMissing
    ) where

import Data.Map.Strict.Internal
