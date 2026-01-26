{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

-- |
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
--
-- = Description
--
-- This module defines common constructs used by both "Data.IntSet" and
-- "Data.IntMap".
--
-- @since 0.8
--

module Data.IntSet.Internal.IntTreeCommons
  ( Key
  , Prefix(..)
  , nomatch
  , left
  , signBranch
  , TreeTreeBranch(..)
  , treeTreeBranch
  , mask
  , branchMask
  , branchPrefix
  , i2w
  , Order(..)
  ) where

import Data.Bits (Bits(..), countLeadingZeros)
import Utils.Containers.Internal.BitUtil (iShiftRL)

#ifdef __GLASGOW_HASKELL__
#  if __GLASGOW_HASKELL__ >= 914
import Language.Haskell.TH.Lift (Lift)
#  else
import Language.Haskell.TH.Syntax (Lift)
-- See Note [ Template Haskell Dependencies ]
import Language.Haskell.TH ()
#  endif
#endif


type Key = Int

-- | A @Prefix@ represents some prefix of high-order bits of an @Int@.
--
-- A @Prefix@ is usually considered in the context of a
-- 'Data.IntSet.Internal.Bin' or 'Data.IntMap.Internal.Bin'.

-- See Note [IntSet structure and invariants] in Data.IntSet.Internal and
-- Note [IntMap structure and invariants] in Data.IntMap.Internal for details.
newtype Prefix = Prefix { unPrefix :: Int }
  deriving Eq

#ifdef __GLASGOW_HASKELL__
deriving instance Lift Prefix
#endif

-- | Whether the @Int@ does not start with the given @Prefix@.
--
-- An @Int@ starts with a @Prefix@ if it shares the high bits with the internal
-- @Int@ value of the @Prefix@ up to the mask bit.
--
-- @nomatch@ is usually used to determine whether a key belongs in a @Bin@,
-- since all keys in a @Bin@ share a @Prefix@.
nomatch :: Int -> Prefix -> Bool
nomatch i p = (i `xor` px) .&. prefixMask /= 0
  where
    px = unPrefix p
    prefixMask = px `xor` (-px)
{-# INLINE nomatch #-}

-- | Whether the @Int@ is to the left of the split created by a @Bin@ with this
-- @Prefix@.
--
-- This does not imply that the @Int@ belongs in this @Bin@. That fact is
-- usually determined first using @nomatch@.
left :: Int -> Prefix -> Bool
left i p = i2w i < i2w (unPrefix p)
{-# INLINE left #-}

-- | A @TreeTreeBranch@ is returned by 'treeTreeBranch' to indicate how two
-- @Bin@s relate to each other.
--
-- Consider that @A@ and @B@ are the @Bin@s whose @Prefix@es are given to
-- @treeTreeBranch@ as the first and second arguments respectively.
data TreeTreeBranch
  = ABL  -- ^ A contains B in the left child
  | ABR  -- ^ A contains B in the right child
  | BAL  -- ^ B contains A in the left child
  | BAR  -- ^ B contains A in the right child
  | EQL  -- ^ A and B have equal prefixes
  | NOM  -- ^ A and B have prefixes that do not match

-- | Calculates how two @Bin@s relate to each other by comparing their
-- @Prefix@es.

-- Notes:
-- * pw .|. (pw-1) sets every bit below the mask bit to 1. This is the greatest
--   key the Bin can have.
-- * pw .&. (pw-1) sets the mask bit and every bit below it to 0. This is the
--   smallest key the Bin can have.
--
-- First, we compare the prefixes to each other. Then we compare a prefix
-- against the greatest/smallest keys the other prefix's Bin could have. This is
-- enough to determine how the two Bins relate to each other. The conditions can
-- be stated as:
--
-- * If pw1 from Bin A is less than pw2 from Bin B, and pw2 is <= the greatest
--   key of Bin A, then Bin A contains Bin B in its right child.
-- * ...and so on

treeTreeBranch :: Prefix -> Prefix -> TreeTreeBranch
treeTreeBranch p1 p2 = case compare pw1 pw2 of
  LT | pw2 <= greatest pw1 -> ABR
     | smallest pw2 <= pw1 -> BAL
     | otherwise           -> NOM
  GT | pw1 <= greatest pw2 -> BAR
     | smallest pw1 <= pw2 -> ABL
     | otherwise           -> NOM
  EQ                       -> EQL
  where
    pw1 = i2w (unPrefix p1)
    pw2 = i2w (unPrefix p2)
    greatest pw = pw .|. (pw-1)
    smallest pw = pw .&. (pw-1)
{-# INLINE treeTreeBranch #-}

-- | Whether this @Prefix@ splits a @Bin@ at the sign bit.
--
-- This can only be True at the top level.
-- If it is true, the left child contains non-negative keys and the right child
-- contains negative keys.
signBranch :: Prefix -> Bool
signBranch p = unPrefix p == (minBound :: Int)
{-# INLINE signBranch #-}

-- | The prefix of @Int@ @i@ up to the switching bit @m@.
mask :: Int -> Int -> Prefix
mask i m = Prefix ((i .&. negate m) .|. m)
{-# INLINE mask #-}

-- | The first switching bit where the two @Int@s disagree.
--
-- Precondition for defined behavior: i1 /= i2
branchMask :: Int -> Int -> Int
branchMask i1 i2 = iShiftRL (minBound :: Int) (countLeadingZeros (i1 `xor` i2))
{-# INLINE branchMask #-}

-- | The shared prefix of two @Int@s.
--
-- Precondition for defined behavior: i1 /= i2
branchPrefix :: Int -> Int -> Prefix
branchPrefix i1 i2 = Prefix ((i1 .|. i2) .&. m)
  where
    m = unsafeShiftR (minBound :: Int) (countLeadingZeros (i1 `xor` i2))
{-# INLINE branchPrefix #-}

i2w :: Int -> Word
i2w = fromIntegral
{-# INLINE i2w #-}

-- Used to compare IntSets and IntMaps
data Order
  = A_LT_B     -- holds for [0,3,4] [0,3,5,1]
  | A_Prefix_B -- holds for [0,3,4] [0,3,4,5]
  | A_EQ_B     -- holds for [0,3,4] [0,3,4]
  | B_Prefix_A -- holds for [0,3,4] [0,3]
  | A_GT_B     -- holds for [0,3,4] [0,2,5]

{--------------------------------------------------------------------
  Notes
--------------------------------------------------------------------}

-- Note [INLINE bit fiddling]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- It is essential that the bit fiddling functions like nomatch, mask,
-- branchMask etc are inlined. If they do not, the memory allocation skyrockets.
-- The GHC usually gets it right, but it is disastrous if it does not. Therefore
-- we explicitly mark these functions INLINE.
