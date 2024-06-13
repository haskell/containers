{-# LANGUAGE CPP #-}
#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Safe #-}
#endif

-- | A strict triple

module Utils.Containers.Internal.StrictTriple (StrictTriple(..)) where

-- | The same as a regular Haskell tuple, but
--
-- @
-- StrictTriple x y _|_ = StrictTriple x _|_ z = StrictTriple _|_ y z = _|_
-- @
data StrictTriple a b c = StrictTriple !a !b !c
