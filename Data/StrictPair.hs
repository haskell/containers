module Data.StrictPair (StrictPair(..), toPair) where

-- | Same as regular Haskell pairs, but (x :*: _|_) = (_|_ :*: y) =
-- _|_
data StrictPair a b = !a :*: !b

toPair :: StrictPair a b -> (a, b)
toPair (x :*: y) = (x, y)
{-# INLINE toPair #-}