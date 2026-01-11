{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE Safe #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
--
-- = Finite Maps (lazy interface)
--
-- This module re-exports the value lazy "Data.Map.Lazy" API.
--
-- The @'Map' k v@ type represents a finite map (sometimes called a dictionary)
-- from keys of type @k@ to values of type @v@. Most operations require that @k@
-- have an instance of the 'Ord' class. A 'Map' is strict in its keys but lazy
-- in its values.
--
-- The functions in "Data.Map.Strict" are careful to force values before
-- installing them in a 'Map'. This is usually more efficient in cases where
-- laziness is not essential. The functions in this module do not do so.
--
-- When deciding if this is the correct data structure to use, consider:
--
-- * If you are using 'Prelude.Int' keys, you will get much better performance for most
-- operations using "Data.IntMap.Lazy".
--
-- * If you don't care about ordering, consider using @Data.HashMap.Lazy@ from the
-- <https://hackage.haskell.org/package/unordered-containers unordered-containers>
-- package instead.
--
-- For a walkthrough of the most commonly used functions see the
-- <https://haskell-containers.readthedocs.io/en/latest/map.html maps introduction>.
--
-- This module is intended to be imported qualified, to avoid name clashes with
-- Prelude functions, e.g.
--
-- > import Data.Map (Map)
-- > import qualified Data.Map as Map
--
-- The @'Ord' k@ instance is expected to be lawful and define a total order.
-- Unless otherwise specified, operations expect equality on keys to be
-- extensional: if keys @k1@ and @k2@ satisfy @k1 == k2@, they are considered
-- identical. For instance, if only one key must be retained by an operation, it
-- is free to select either.
--
--
-- == Warning
--
-- The size of a 'Map' must not exceed @'Prelude.maxBound' :: 'Prelude.Int'@.
-- Violation of this condition is not detected and if the size limit is exceeded,
-- its behaviour is undefined.
--
--
-- == Implementation
--
-- The implementation of 'Map' is based on /size balanced/ binary trees (or
-- trees of /bounded balance/) as described by:
--
--    * Stephen Adams, \"/Efficient sets—a balancing act/\",
--      Journal of Functional Programming 3(4):553-562, October 1993,
--      <https://doi.org/10.1017/S0956796800000885>,
--      <https://groups.csail.mit.edu/mac/users/adams/BB/index.html>.
--    * J. Nievergelt and E.M. Reingold,
--      \"/Binary search trees of bounded balance/\",
--      SIAM journal of computing 2(1), March 1973.
--      <https://doi.org/10.1137/0202005>.
--    * Yoichi Hirai and Kazuhiko Yamamoto,
--      \"/Balancing weight-balanced trees/\",
--      Journal of Functional Programming 21(3):287-307, 2011,
--      <https://doi.org/10.1017/S0956796811000104>
--
--  Bounds for 'union', 'intersection', and 'difference' are as given
--  by
--
--    * Guy Blelloch, Daniel Ferizovic, and Yihan Sun,
--      \"/Parallel Ordered Sets Using Join/\",
--      <https://arxiv.org/abs/1602.02120v4>.
--
--
-- == Performance information
--
-- The time complexity is given for each operation in
-- [big-O notation](http://en.wikipedia.org/wiki/Big_O_notation), with \(n\)
-- referring to the number of entries in the map.
--
-- Operations like 'lookup', 'insert', and 'delete' take \(O(\log n)\) time.
--
-- Binary set operations like 'union' and 'intersection' take
-- \(O\bigl(m \log\bigl(\frac{n}{m}+1\bigr)\bigr)\) time, where \(m\) and \(n\)
-- are the sizes of the smaller and larger input maps respectively.
--
-----------------------------------------------------------------------------

module Data.Map
    ( module Data.Map.Lazy
    ) where

import Data.Map.Lazy
