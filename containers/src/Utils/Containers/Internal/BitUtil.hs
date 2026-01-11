{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Trustworthy #-}
#endif

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
    ( shiftLL
    , shiftRL
    , wordSize
    , iShiftRL
    ) where

import Data.Bits (unsafeShiftL, unsafeShiftR, finiteBitSize)
#ifdef __GLASGOW_HASKELL__
import GHC.Exts (Int(..), uncheckedIShiftRL#)
#endif

-- Right and left logical shifts.
--
-- Precondition for defined behavior: 0 <= shift amount < wordSize
shiftRL, shiftLL :: Word -> Int -> Word
shiftRL = unsafeShiftR
shiftLL = unsafeShiftL

{-# INLINE wordSize #-}
wordSize :: Int
wordSize = finiteBitSize (0 :: Word)

-- Right logical shift.
--
-- Precondition for defined behavior: 0 <= shift amount < wordSize
iShiftRL :: Int -> Int -> Int
#ifdef __GLASGOW_HASKELL__
iShiftRL (I# x#) (I# sh#) = I# (uncheckedIShiftRL# x# sh#)
#else
iShiftRL x sh = fromIntegral (unsafeShiftR (fromIntegral x :: Word) sh)
#endif
