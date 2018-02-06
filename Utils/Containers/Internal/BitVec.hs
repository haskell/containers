{-# language CPP, MagicHash, PatternGuards #-}
#include "MachDeps.h"

-- -------------------------------------------------------------------
-- |
-- Module      :  Utils.Containers.Internal.BitQueue
-- Copyright   :  (c) David Feuer 2018
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  non-portable
--
-- Compact bit vectors. We use these in Data.Graph.

module Utils.Containers.Internal.BitVec
  (
    BitVecM
  , newBitVecM
  , readBitM
  , setBitM
  , toListM
  ) where

import Data.Array.ST
import Data.Array.Base
import Control.Monad.ST
import Data.Word
import Data.Bits
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

data Index = Index { _whichWord :: !Int, _whichBit :: !Int} deriving Show

divWordSize, remWordSize :: Int -> Int
#if defined(WORD_SIZE_IN_BITS) && (WORD_SIZE_IN_BITS == 64)
divWordSize i = i `unsafeShiftR` 6
remWordSize i = i .&. 0x3f
#elif defined(WORD_SIZE_IN_BITS) && (WORD_SIZE_IN_BITS == 32)
divWordSize i = i `unsafeShiftR` 5
remWordSize i = i .&. 0x1f
#else
divWordSize i = case wordSize of
  64 -> i `unsafeShiftR` 6
  32 -> i `unsafeShiftR` 5
  _  -> i `quot` wordSize

remWordSize i = case wordSize of
  64 -> i .&. 0x3f
  32 -> i .&. 0x1f
  _  -> i `rem` wordSize
#endif

wordSize :: Int
wordSize = finiteBitSize (0 :: Word)

getIndex :: Int -> Index
getIndex i = Index (divWordSize i) (remWordSize i)

newtype BitVecM s = BitVecM (STUArray s Int Word64)

-- | Convert a bit vector to a list, most significant bit first.
-- This function is intended for debugging.
toListM :: Int -- ^ The size of the bit vector
        -> BitVecM s -- ^ The bit vector
        -> ST s [Bool]
toListM n bv = go (n - 1)
  where
    go i | i < 0 = pure []
    go i = (:) <$> readBitM bv i <*> go (i - 1)

-- | Read a bit in a bit vector
readBitM :: BitVecM s  -- ^ Bit vector
         -> Int        -- ^ Index
         -> ST s Bool
readBitM (BitVecM v) i
  | Index wi bi <- getIndex i
  = fmap (`testBit` bi) (unsafeRead v wi)

-- | Set a bit in a bit vector to True.
setBitM :: BitVecM s  -- ^ Bit vector
        -> Int        -- ^ Index
        -> ST s ()
setBitM (BitVecM v) i
  | Index wi bi <- getIndex i
  = do
      oldw <- unsafeRead v wi
      let -- We'd much rather spell this `bit bi`, but then GHC
          -- uses a "safe" shift, which we really don't need.
          theBit = 1 `unsafeShiftL` bi
      let neww = oldw .|. theBit
      unsafeWrite v wi neww

-- | Create a bit vector of the given size, with all False values.
newBitVecM :: Int -- ^ Desired size
           -> ST s (BitVecM s)
newBitVecM s = do
  arr <- newArray (0, divWordSize (s + wordSize - 1)) 0
  return (BitVecM arr)
