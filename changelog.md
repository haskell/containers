# Changelog for [`containers` package](http://github.com/haskell/containers)

## 0.5.7.1  *Dec 2015*

  * Planned to bundle with GHC 8.0.1.

  * Add `IsString` instance to `Data.Sequence`.

  * Define `Semigroup` instances for ``Data.Map`, `Data.Set`, `Data.IntMap`,
    `Data.IntSet` and `Data.Sequence`.

## 0.5.6.2  *Dec 2014*

  * Bundled with GHC 7.10.1.

  * Add role annotations for `Data.Map` and `Data.Set`.

  * Add `IsList` instances for `Data.Map`, `Data.Set`, `Data.IntMap` and
    `Data.IntSet`.

  * Several performance improvements for `Data.Sequence`.

  * Add `Data.Sequence.fromFunction` and `Data.Sequence.fromArray`.

## 0.5.4.0  *Jan 2014*

  * Bundled with GHC 7.8.1.

  * The `Data.Map.fromList` and `Data.Set.fromList` now use linear-time
    algorithm if the input is sorted, without need to call `fromDistinctAscList`.

  * Implement indexing operations (`lookupIndex`, `findIndex`, `elemAt`,
    `deletaAt`) for `Data.Set` too.

  * Add `Applicative` and `Alternative` instances for `Data.Sequence`.

  * Add `foldMapWithKey` to `Data.Map` and `Data.IntMap`.

  * Implement poly-kinded `Typeable`.

  * Add `Functor` instance for `Data.Graph.SCC`.

  * Add `Data.Map.splitRoot` and `Data.Set.splitRoot`.

## 0.5.0.0  *May 2012*

  * Bundled with GHC 7.6.1.

  * Major improvements since last release:
    * a clearer distinction between value-lazy and value-strict containers,
    * performance improvements across the board,
    * a big internal clean-up, and
    * new functions for e.g. merging, updating, and searching containers.

  * While the old `Data.Map` and
    `Data.IntMap` modules will continue to exist for the foreseeable future, we've
    abandoned the practice of having the strict and lazy versions of each
    function distinguished by an apostrophe. The distinction is instead made at
    the module level, by introducing four new modules:
    * Data.Map.Strict
    * Data.Map.Lazy
    * Data.IntMap.Strict
    * Data.IntMap.Lazy

    This split has three benefits:
    * It makes the choice between value-strict and value-lazy containers
      more declarative; you pick once at import time, instead of having to
      remember to use the strict or lazy versions of a function every time
      you modify the container.
    * It alleviates a common source of performance issues, by forcing the
      user to think about the strictness properties upfront. For example,
      using insertWith instead of insertWith' is a common source of
      containers-related performance bugs.
    * There are fewer functions per module, making it easier to get an
      overview of each module.

  * Note that the types used in the strict and lazy APIs are the same, so
    you can still use the same container in a "mixed" manner, if needed.

  * The `Data.IntSet` representation changed to store small sets using
    bits in an `Word`. Larger sets are stored as a collection of such
    dense small sets, connected together by a prefix trie.

## 0.4.2.1  *Feb 2012*

  * Bundled with GHC 7.4.1.

  * `Data.Map now exports `foldr`, `foldr'`, `foldl` and `foldl'`.

  * `Data.Set now exports `foldr`, `foldr'`, `foldl` and `foldl'`.

  * `Data.IntMap now exports `foldr`, `foldr'`, `foldl`, `foldl'`, `foldrWithKey`, `foldrWithKey'`, `foldlWithKey` and `foldlWithKey'`.

  * `Data.IntSet now exports `foldr`, `foldr'`, `foldl` and `foldl'`.

  * `Data.Map.foldWithKey` is no longer deprecated, although it is expected to be deprecated again in the future.

  * There are now `NFData` instance for `Data.Map.Map`, `Data.Set.Set`, `Data.IntMap.IntMap`, `Data.IntSet.IntSet` and `Data.Tree.Tree`.

## 0.4.1.0  *Aug 2011*

  * Bundled with GHC 7.2.1.

  * `Data.Map` now exports new functions `foldrWithKey'` and `foldlWithKey'`, which are strict variants of `foldrWithKey` and `foldlWithKey` respectively.

  * `Data.IntMap` now exports new functions `insertWith'` and `insertWithKey'`, which are strict variants of `insertWith` and `insertWithKey` respectively.

## 0.4.0.0  *Nov 2010*

  * Bundled with GHC 7.0.1.

  * Strictness is now more consistent, with containers being strict in their elements even in singleton cases.

  * There is a new function `insertLookupWithKey'` in `Data.Map`.

  * The `foldWithKey` function in `Data.Map` has been deprecated in favour of `foldrWithKey`.

## 0.3.0.0  *Dec 2009*

  * Bundled with GHC 6.12.1.

  * `mapAccumRWithKey` has been added to `Data.IntMap`.

  * A `Traversable` instance has been added to `Data.IntMap.IntMap`.

  * The types of `Data.IntMap.intersectionWith` and `Data.IntMap.intersectionWithKey` have been changed from
    `intersectionWith :: (a -> b -> a) -> IntMap a -> IntMap b -> IntMap a`
    `intersectionWithKey :: (Key -> a -> b -> a) -> IntMap a -> IntMap b -> IntMap a`
    to
    `intersectionWith :: (a -> b -> c) -> IntMap a -> IntMap b -> IntMap c`
    `intersectionWithKey :: (Key -> a -> b -> c) -> IntMap a -> IntMap b -> IntMap c`

  * The types of `Data.IntMap.findMin` and `Data.IntMap.findMax` have been changed from
    `findMin :: IntMap a -> a`
    `findMax :: IntMap a -> a`
    to
    `findMin :: IntMap a -> (Int,a)`
    `findMax :: IntMap a -> (Int,a)`

  * `Data.Map` now exports `mapAccumRWithKey`, `foldrWithKey`, `foldlWithKey` and `toDescList`.

  * `Data.Sequence` now exports `replicate`, `replicateA`, `replicateM`, `iterateN`, `unfoldr`, `unfoldl`, `scanl`, `scanl1`, `scanr`, `scanr1`, `tails`, `inits`, `takeWhileL`, `takeWhileR`, `dropWhileL`, `dropWhileR`, `spanl`, `spanr`, `breakl`, `breakr`, `partition`, `filter`, `sort`, `sortBy`, `unstableSort`, `unstableSortBy`, `elemIndexL`, `elemIndicesL`, `elemIndexR`, `elemIndicesR`, `findIndexL`, `findIndicesL`, `findIndexR`, `findIndicesR`, `foldlWithIndex`, `foldrWithIndex`, `mapWithIndex`, `zip`, `zipWith`, `zip3`, `zipWith3`, `zip4` and `zipWith4`.

## 0.2.0.0  *Nov 2008*

  * Bundled with GHC 6.10.1.

  * Various result type now use `Maybe` rather than allowing any `Monad`.

## 0.1.0.0  *Nov 2007*

  * Bundled with GHC 6.8.1.

  * Initial split off from GHC base.
