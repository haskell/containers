{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE MagicHash #-}
#endif
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
    ( bitcount
    , highestBitMask
    , shiftLL
    , shiftRL
    , wordSize
    ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Bits ((.|.), xor)
#endif
import Data.Bits (popCount, unsafeShiftL, unsafeShiftR
#if MIN_VERSION_base(4,8,0)
    , countLeadingZeros
#endif
    )
#if MIN_VERSION_base(4,7,0)
import Data.Bits (finiteBitSize)
#else
import Data.Bits (bitSize)
#endif

#if !MIN_VERSION_base (4,8,0)
import Data.Word (Word)
#endif

{----------------------------------------------------------------------
  [bitcount] as posted by David F. Place to haskell-cafe on April 11, 2006,
  based on the code on
  http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetKernighan,
  where the following source is given:
    Published in 1988, the C Programming Language 2nd Ed. (by Brian W.
    Kernighan and Dennis M. Ritchie) mentions this in exercise 2-9. On April
    19, 2006 Don Knuth pointed out to me that this method "was first published
    by Peter Wegner in CACM 3 (1960), 322. (Also discovered independently by
    Derrick Lehmer and published in 1964 in a book edited by Beckenbach.)"
----------------------------------------------------------------------}

bitcount :: Int -> Word -> Int
bitcount a x = a + popCount x
{-# INLINE bitcount #-}

-- The highestBitMask implementation is based on
-- http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
-- which has been put in the public domain.

-- | Return a word where only the highest bit is set.
highestBitMask :: Word -> Word
#if MIN_VERSION_base(4,8,0)
highestBitMask w = shiftLL 1 (wordSize - 1 - countLeadingZeros w)
#else
highestBitMask x1 = let x2 = x1 .|. x1 `shiftRL` 1
                        x3 = x2 .|. x2 `shiftRL` 2
                        x4 = x3 .|. x3 `shiftRL` 4
                        x5 = x4 .|. x4 `shiftRL` 8
                        x6 = x5 .|. x5 `shiftRL` 16
#if !(defined(__GLASGOW_HASKELL__) && WORD_SIZE_IN_BITS==32)
                        x7 = x6 .|. x6 `shiftRL` 32
                     in x7 `xor` (x7 `shiftRL` 1)
#else
                     in x6 `xor` (x6 `shiftRL` 1)
#endif
#endif
{-# INLINE highestBitMask #-}

-- Right and left logical shifts.
shiftRL, shiftLL :: Word -> Int -> Word
shiftRL = unsafeShiftR
shiftLL = unsafeShiftL

{-# INLINE wordSize #-}
wordSize :: Int
#if MIN_VERSION_base(4,7,0)
wordSize = finiteBitSize (0 :: Word)
#else
wordSize = bitSize (0 :: Word)
#endif
