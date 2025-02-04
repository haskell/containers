{-# OPTIONS_GHC -ddump-prep #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module defines efficient representations of GADTs that are shaped
-- like (strict) unary natural numbers. That is, each type looks, from the
-- outside, something like this:
--
-- @
-- data NatLike ... where
--   ZeroLike :: NatLike ...
--   SuccLike :: !(NatLike ...) -> NatLike ...
-- @
--
-- but in fact it is represented by a single machine word. We put these in a
-- separate module to confine the highly unsafe magic used in the
-- implementation.
--
-- Caution: Unlike the GADTs they represent, the types in this module are
-- bounded by @maxBound \@Word@, and attempting to take a successor of the
-- maximum bound will throw an overflow error. That's okay for our purposes
-- of implementing certain functions in "Data.Sequence.Internal"—the spine
-- of a well-formed sequence can only reach a length of around the word
-- size, not even close to @maxBound \@Word@.

module Data.Sequence.Internal.Depth
  ( Depth_ (Bottom, Deeper)
  , Depth2_ (Bottom2, Deeper2)
  ) where

import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)

-- @Depth_@ is an optimized representation of the following GADT:
--
-- @
-- data Depth_ node a t where
--   Bottom :: Depth_ node a a
--   Deeper :: !(Depth_ node a t) -> Depth_ node a (node t)
-- @
--
-- "Data.Sequence.Internal" fills in the @node@ parameter with its @Node@
-- constructor; we have to be more general in this module because we don't
-- have access to that.
--
-- @Depth_@ is represented internally as a 'Word' for performance, and the
-- 'Bottom' and 'Deeper' pattern synonyms implement the above GADT interface.
-- The implementation is "safe"—in the very unlikely event of arithmetic
-- overflow, an error will be thrown. This decision is subject to change;
-- arithmetic overflow on 64-bit systems requires somewhat absurdly long
-- computations on sequences constructed with extensive amounts of internal
-- sharing (e.g., using the '*>' operator repeatedly).
newtype Depth_ (node :: Type -> Type) (a :: Type) (t :: Type)
  = Depth_ Word
type role Depth_ nominal nominal nominal

-- | The depth is 0.
pattern Bottom :: () => t ~ a => Depth_ node a t
pattern Bottom <- (checkBottom -> AtBottom)
  where
    Bottom = Depth_ 0

-- | The depth is non-zero.
pattern Deeper :: () => t ~ node t' => Depth_ node a t' -> Depth_ node a t
pattern Deeper d <- (checkBottom -> NotBottom d)
  where
    Deeper (Depth_ d)
      | d == maxBound = error "Depth overflow"
      | otherwise = Depth_ (d + 1)

{-# COMPLETE Bottom, Deeper #-}

data CheckedBottom node a t where
  AtBottom :: CheckedBottom node a a
  NotBottom :: !(Depth_ node a t) -> CheckedBottom node a (node t)

checkBottom :: Depth_ node a t -> CheckedBottom node a t
checkBottom (Depth_ 0) = unsafeCoerce AtBottom
checkBottom (Depth_ d) = unsafeCoerce (NotBottom (Depth_ (d - 1)))


-- | A version of 'Depth_' for implementing traversals. Conceptually,
--
-- @
-- data Depth2_ node a t b u where
--   Bottom2 :: Depth_ node a a b b
--   Deeper2 :: !(Depth_ node a t b u) -> Depth_ node a (node t) b (node u)
-- @
newtype Depth2_ (node :: Type -> Type) (a :: Type) (t :: Type) (b :: Type) (u :: Type)
  = Depth2_ Word
type role Depth2_ nominal nominal nominal nominal nominal

-- | The depth is 0.
pattern Bottom2 :: () => (t ~ a, u ~ b) => Depth2_ node a t b u
pattern Bottom2 <- (checkBottom2 -> AtBottom2)
  where
    Bottom2 = Depth2_ 0

-- | The depth is non-zero.
pattern Deeper2 :: () => (t ~ node t', u ~ node u') => Depth2_ node a t' b u' -> Depth2_ node a t b u
pattern Deeper2 d <- (checkBottom2 -> NotBottom2 d)
  where
    Deeper2 (Depth2_ d)
      | d == maxBound = error "Depth2 overflow"
      | otherwise = Depth2_ (d + 1)

{-# COMPLETE Bottom2, Deeper2 #-}

data CheckedBottom2 node a t b u where
  AtBottom2 :: CheckedBottom2 node a a b b
  NotBottom2 :: !(Depth2_ node a t b u) -> CheckedBottom2 node a (node t) b (node u)

checkBottom2 :: Depth2_ node a t b u -> CheckedBottom2 node a t b u
checkBottom2 (Depth2_ 0) = unsafeCoerce AtBottom2
checkBottom2 (Depth2_ d) = unsafeCoerce (NotBottom2 (Depth2_ (d - 1)))
