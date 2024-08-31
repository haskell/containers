{-# LANGUAGE CPP #-}
#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Safe #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Utils.Containers.Internal.BitUtil
-- Copyright   :  (c) Clark Gaebel 2012
--                (c) Johan Tibel 2012
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
-----------------------------------------------------------------------------
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.

module Utils.Containers.Internal.BitUtil
    ( highestBitMask
    , shiftLL
    , shiftRL
    , wordSize
    ) where

import Data.Bits (unsafeShiftL, unsafeShiftR
    , countLeadingZeros, finiteBitSize
    )

-- | Return a word where only the highest bit is set.
highestBitMask :: Word -> Word
highestBitMask w = shiftLL 1 (wordSize - 1 - countLeadingZeros w)
{-# INLINE highestBitMask #-}

-- Right and left logical shifts.
shiftRL, shiftLL :: Word -> Int -> Word
shiftRL = unsafeShiftR
shiftLL = unsafeShiftL

{-# INLINE wordSize #-}
wordSize :: Int
wordSize = finiteBitSize (0 :: Word)
