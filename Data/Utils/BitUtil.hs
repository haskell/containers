{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE MagicHash #-}
#endif
#if !defined(TESTING) && __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Trustworthy #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Utils.BitUtil
-- Copyright   :  (c) Clark Gaebel 2012
--                (c) Johan Tibel 2012
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
-----------------------------------------------------------------------------

module Data.Utils.BitUtil
    ( highestBitMask
    , shiftLL
    , shiftRL
    ) where

import Data.Bits ((.|.), xor)

#if __GLASGOW_HASKELL__
import GHC.Exts (Word(..), Int(..))
import GHC.Prim (uncheckedShiftL#, uncheckedShiftRL#)
#else
import Data.Word (shiftL, shiftR)
#endif

-- The highestBitMask implementation is based on
-- http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
-- which has been put in the public domain.

-- | Return a word where only the highest bit is set.
highestBitMask :: Word -> Word
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
{-# INLINE highestBitMask #-}

-- Right and left logical shifts.
shiftRL, shiftLL :: Word -> Int -> Word
#if __GLASGOW_HASKELL__
{--------------------------------------------------------------------
  GHC: use unboxing to get @shiftRL@ inlined.
--------------------------------------------------------------------}
shiftRL (W# x) (I# i) = W# (uncheckedShiftRL# x i)
shiftLL (W# x) (I# i) = W# (uncheckedShiftL#  x i)
#else
shiftRL x i   = shiftR x i
shiftLL x i   = shiftL x i
#endif
{-# INLINE shiftRL #-}
{-# INLINE shiftLL #-}
