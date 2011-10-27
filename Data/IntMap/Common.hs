{-# LANGUAGE NoBangPatterns, ScopedTypeVariables #-}
#if !defined(TESTING) && __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMap.Common
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This defines the data structures and core (hidden) manipulations
-- on representations.
-----------------------------------------------------------------------------

-- It is essential that the bit fiddling functions like mask, zero, branchMask
-- etc are inlined. If they do not, the memory allocation skyrockets. The GHC
-- usually gets it right, but it is disastrous if it does not. Therefore we
-- explicitly mark these functions INLINE.

module Data.IntMap.Common (
            -- * Map type
              IntMap(..), Key          -- instance Eq,Show

            -- * Internal types
            , Mask, Prefix, Nat

            -- * Utility
            , natFromInt
            , intFromNat
            , shiftRL
            , join
            , bin
            , zero
            , nomatch
            , match
            , mask
            , maskW
            , shorter
            , branchMask
            , highestBitMask
            , foldlStrict
            ) where

import Prelude hiding (lookup,map,filter,foldr,foldl,null)
import Data.Bits

#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts ( Word(..), Int(..), shiftRL# )
#elif __GLASGOW_HASKELL__
import Word
import GlaExts ( Word(..), Int(..), shiftRL# )
#else
import Data.Word
#endif

-- A "Nat" is a natural machine word (an unsigned Int)
type Nat = Word

natFromInt :: Key -> Nat
natFromInt = fromIntegral
{-# INLINE natFromInt #-}

intFromNat :: Nat -> Key
intFromNat = fromIntegral
{-# INLINE intFromNat #-}

shiftRL :: Nat -> Key -> Nat
#if __GLASGOW_HASKELL__
{--------------------------------------------------------------------
  GHC: use unboxing to get @shiftRL@ inlined.
--------------------------------------------------------------------}
shiftRL (W# x) (I# i)
  = W# (shiftRL# x i)
#else
shiftRL x i   = shiftR x i
{-# INLINE shiftRL #-}
#endif

{--------------------------------------------------------------------
  Types
--------------------------------------------------------------------}

-- The order of constructors of IntMap matters when considering performance.
-- Currently in GHC 7.0, when type has 3 constructors, they are matched from
-- the first to the last -- the best performance is achieved when the
-- constructors are ordered by frequency.
-- On GHC 7.0, reordering constructors from Nil | Tip | Bin to Bin | Tip | Nil
-- improves the containers_benchmark by 9.5% on x86 and by 8% on x86_64.

-- | A map of integers to values @a@.
data IntMap a = Bin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask !(IntMap a) !(IntMap a)
              | Tip {-# UNPACK #-} !Key a
              | Nil

type Prefix = Int
type Mask   = Int
type Key    = Int

{--------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------}
{--------------------------------------------------------------------
  Join
--------------------------------------------------------------------}
join :: Prefix -> IntMap a -> Prefix -> IntMap a -> IntMap a
join p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m
{-# INLINE join #-}

{--------------------------------------------------------------------
  @bin@ assures that we never have empty trees within a tree.
--------------------------------------------------------------------}
bin :: Prefix -> Mask -> IntMap a -> IntMap a -> IntMap a
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r   = Bin p m l r
{-# INLINE bin #-}

  
{--------------------------------------------------------------------
  Endian independent bit twiddling
--------------------------------------------------------------------}
zero :: Key -> Mask -> Bool
zero i m
  = (natFromInt i) .&. (natFromInt m) == 0
{-# INLINE zero #-}

nomatch,match :: Key -> Prefix -> Mask -> Bool
nomatch i p m
  = (mask i m) /= p
{-# INLINE nomatch #-}

match i p m
  = (mask i m) == p
{-# INLINE match #-}

mask :: Key -> Mask -> Prefix
mask i m
  = maskW (natFromInt i) (natFromInt m)
{-# INLINE mask #-}


{--------------------------------------------------------------------
  Big endian operations  
--------------------------------------------------------------------}
maskW :: Nat -> Nat -> Prefix
maskW i m
  = intFromNat (i .&. (complement (m-1) `xor` m))
{-# INLINE maskW #-}

shorter :: Mask -> Mask -> Bool
shorter m1 m2
  = (natFromInt m1) > (natFromInt m2)
{-# INLINE shorter #-}

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  = intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))
{-# INLINE branchMask #-}

{----------------------------------------------------------------------
  Finding the highest bit (mask) in a word [x] can be done efficiently in
  three ways:
  * convert to a floating point value and the mantissa tells us the 
    [log2(x)] that corresponds with the highest bit position. The mantissa 
    is retrieved either via the standard C function [frexp] or by some bit 
    twiddling on IEEE compatible numbers (float). Note that one needs to 
    use at least [double] precision for an accurate mantissa of 32 bit 
    numbers.
  * use bit twiddling, a logarithmic sequence of bitwise or's and shifts (bit).
  * use processor specific assembler instruction (asm).

  The most portable way would be [bit], but is it efficient enough?
  I have measured the cycle counts of the different methods on an AMD 
  Athlon-XP 1800 (~ Pentium III 1.8Ghz) using the RDTSC instruction:

  highestBitMask: method  cycles
                  --------------
                   frexp   200
                   float    33
                   bit      11
                   asm      12

  highestBit:     method  cycles
                  --------------
                   frexp   195
                   float    33
                   bit      11
                   asm      11

  Wow, the bit twiddling is on today's RISC like machines even faster
  than a single CISC instruction (BSR)!
----------------------------------------------------------------------}

{----------------------------------------------------------------------
  [highestBitMask] returns a word where only the highest bit is set.
  It is found by first setting all bits in lower positions than the 
  highest bit and than taking an exclusive or with the original value.
  Allthough the function may look expensive, GHC compiles this into
  excellent C code that subsequently compiled into highly efficient
  machine code. The algorithm is derived from Jorg Arndt's FXT library.
----------------------------------------------------------------------}
highestBitMask :: Nat -> Nat
highestBitMask x0
  = case (x0 .|. shiftRL x0 1) of
     x1 -> case (x1 .|. shiftRL x1 2) of
      x2 -> case (x2 .|. shiftRL x2 4) of
       x3 -> case (x3 .|. shiftRL x3 8) of
        x4 -> case (x4 .|. shiftRL x4 16) of
         x5 -> case (x5 .|. shiftRL x5 32) of   -- for 64 bit platforms
          x6 -> (x6 `xor` (shiftRL x6 1))
{-# INLINE highestBitMask #-}


{--------------------------------------------------------------------
  Utilities 
--------------------------------------------------------------------}

foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f = go
  where
    go z []     = z
    go z (x:xs) = let z' = f z x in z' `seq` go z' xs
{-# INLINE foldlStrict #-}
