-- | Simple strict types for internal use.
module Utils.Containers.Internal.Strict
  ( StrictPair(..)
  , toPair
  , StrictTriple(..)
  ) where

-- | The same as a regular Haskell pair, but
--
-- @
-- (x :*: _|_) = (_|_ :*: y) = _|_
-- @
data StrictPair a b = !a :*: !b

infixr 1 :*:

-- | Convert a strict pair to a standard pair.
toPair :: StrictPair a b -> (a, b)
toPair (x :*: y) = (x, y)
{-# INLINE toPair #-}

data StrictTriple a b c = TripleS !a !b !c
