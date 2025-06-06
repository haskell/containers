# Changelog for [`containers` package](http://github.com/haskell/containers)

## Next release

### Additions

* Add `compareSize` for `IntSet` and `IntMap`. (Soumik Sarkar)
  ([#1135](https://github.com/haskell/containers/pull/1135))

### Performance improvements

* Improved performance for `Data.IntMap.restrictKeys` and
  `Data.IntMap.withoutKeys`. (Soumik Sarkar)
  ([#1131](https://github.com/haskell/containers/pull/1131))

## 0.8  *March 2025*

### Breaking changes

* `Data.IntMap.Lazy.split`, `Data.IntMap.Strict.split`,
  `Data.IntMap.Lazy.splitLookup`, `Data.IntMap.Strict.splitLookup` and
  `Data.IntSet.splitMember` are now strict in the key. Previously, the key was
  ignored for an empty map or set. (Soumik Sarkar)
  ([#982](https://github.com/haskell/containers/pull/982),
  [#983](https://github.com/haskell/containers/pull/983))

* These functions have been updated to match the strictness of their
  `fromList` counterparts:

  * `Data.Map.Strict`: `fromAscList`, `fromAscListWith`, `fromAscListWithKey`
    `fromDescList`, `fromDescListWith`, `fromDescListWithKey`
  * `Data.IntMap.Strict`: `fromAscList`, `fromAscListWith`, `fromAscListWithKey`

  Previously they were lazier and did not force the first value in runs of at
  least 2 entries with equal keys. (Soumik Sarkar)
  ([#1023](https://github.com/haskell/containers/pull/1023))

* `Data.Set.fold` and `Data.IntSet.fold` are deprecated. One should instead use
  `Data.Set.foldr` and `Data.IntSet.foldr`. (Soumik Sarkar)
  ([#1049](https://github.com/haskell/containers/pull/1049))

* For `Data.IntMap.{Lazy,Strict}`, `updateMin`, `updateMax`, `updateMinWithKey`,
  `updateMaxWithKey` now return an empty map for an input empty map instead of
  calling `error`. This matches the behavior of `Data.Map`. (Kushagra Gupta)
  ([#1065](https://github.com/haskell/containers/pull/1065))

* `foldl'` and `foldr'` for `Seq` are now strict in the initial value. This
  matches the behavior of the default implementations and of other structures in
  the library. (Soumik Sarkar)
  ([#1077](https://github.com/haskell/containers/pull/1077))

* Some long deprecated functions, whose definitions currently cause type errors,
  have been removed. (Soumik Sarkar)
  ([#1046](https://github.com/haskell/containers/pull/1046))

### Bug fixes

* Make the package compile with [MicroHs](https://github.com/augustss/MicroHs).
  (Lennart Augustsson)
  ([#1043](https://github.com/haskell/containers/pull/1043),
  [#1081](https://github.com/haskell/containers/pull/1081))

* Fix a strictness bug in `Data.Map.Strict.fromDistinctAscList` and
  `Data.Map.Strict.fromDistinctDescList` where all values were not forced to
  WHNF. This bug affects versions 0.6.8 and 0.7. (Neil Mayhew)
  ([#996](https://github.com/haskell/containers/pull/996))

* Fix a bug in `Data.IntMap`'s `isProperSubmapOfBy` where it could incorrectly
  return `False`. (Soumik Sarkar)
  ([#1008](https://github.com/haskell/containers/pull/1008))

* Make `Data.Map.Merge.{Lazy,Strict}.filterAMissing` sequence effects in the
  correct order. (j6carey)
  ([#1005](https://github.com/haskell/containers/pull/1005))

* `Data.Map.Strict.mergeWithKey` now forces the result of the combining function
  to WHNF. (Soumik Sarkar)
  ([#1024](https://github.com/haskell/containers/pull/1024))

* Fix an issue where `Data.Map.mergeWithKey`, `Data.Map.Strict.mergeWithKey`,
  `Data.IntMap.mergeWithKey`, `Data.IntMap.Strict.mergeWithKey` could call the
  provided `only2` function with empty maps, contrary to documentation.
  (Soumik Sarkar) ([#1025](https://github.com/haskell/containers/pull/1025))

### Additions

* Add `Data.Graph.flattenSCC1`. (Andreas Abel)
  ([#987](https://github.com/haskell/containers/pull/987))

* Add `symmetricDifference` for `Set`, `Map`, `IntSet`, `IntMap`.
  (Soumik Sarkar) ([#1009](https://github.com/haskell/containers/pull/1009))

* Add `lookupMin` and `lookupMax` for `Data.IntSet`. (Soumik Sarkar)
  ([#976](https://github.com/haskell/containers/pull/976))

* Add `Intersection` and `intersections` for `Data.Set` and `Data.IntSet`.
  (Reed Mullanix, Soumik Sarkar)
  ([#756](https://github.com/haskell/containers/pull/756),
  [#1040](https://github.com/haskell/containers/pull/1040),
  [#1052](https://github.com/haskell/containers/pull/1052),
  [#1080](https://github.com/haskell/containers/pull/1080))

* Add `foldMap` for `Data.IntSet`. (Soumik Sarkar)
  ([#1048](https://github.com/haskell/containers/pull/1048))

* Add `filterKeys` for `Data.Map` and `Data.IntMap`. (flip111)
  ([#972](https://github.com/haskell/containers/pull/972))

* `NFData1`, `NFData2` instances for `SCC`, `IntMap`, `Map`, `Sequence`, `Set`,
  `Tree` and relevant internal dependencies (David Beacham)
  ([#992](https://github.com/haskell/containers/pull/992))

* Add `leaves`, `edges`, `pathsToRoot`, `pathsFromRoot`, `PostOrder` to
  `Data.Tree`. (Soumik Sarkar)
  ([#1109](https://github.com/haskell/containers/pull/1109))

### Performance improvements

* The internal representations of `IntMap` and `IntSet` have been changed
  to be a little more memory efficient. Consequently, many functions on
  `IntMap`s and `IntSet`s are a little faster now. (Soumik Sarkar)
  ([#995](https://github.com/haskell/containers/pull/995),
  [#998](https://github.com/haskell/containers/pull/998))

* Improved performance for `Data.Map`'s `minView`, `maxView`, `difference`.
  (Soumik Sarkar) ([#1001](https://github.com/haskell/containers/pull/1001))

* For `Data.Graph.SCC`, `Foldable.toList` and `Foldable1.toNonEmpty` now
  do not perform an unnecessary copy. (Soumik Sarkar)
  ([#1057](https://github.com/haskell/containers/pull/1057))

* Improved performance for `Data.Intset`'s `foldr`, `foldl'`, `foldl`, `foldr'`.
  (Soumik Sarkar) ([#1079](https://github.com/haskell/containers/pull/1079))

* Improved performance for `Data.Set` and `Data.Map`'s `fromAscList*` and
  `fromDescList*` functions. (Soumik Sarkar)
  ([#1083](https://github.com/haskell/containers/pull/1083))

* Improved performance for `Data.Set`'s `fromList`, `map` and `Data.Map`'s
  `fromList`, `fromListWith`, `fromListWithKey`, `mapKeys`, `mapKeysWith`.
  (Soumik Sarkar) ([#1042](https://github.com/haskell/containers/pull/1042))

* Improved performance for many `Set` and `Map` modification operations,
  including `insert` and `delete`, by inlining part of the balancing
  routine. (Soumik Sarkar)
  ([#1056](https://github.com/haskell/containers/pull/1056))

* Improved performance for `Eq` and `Ord` instances of `Set`, `Map`, `IntSet`,
  `IntMap`, `Seq`. (Soumik Sarkar)
  ([#1028](https://github.com/haskell/containers/pull/1028),
  [#1017](https://github.com/haskell/containers/pull/1017),
  [#1035](https://github.com/haskell/containers/pull/1035),
  [#1086](https://github.com/haskell/containers/pull/1086),
  [#1112](https://github.com/haskell/containers/pull/1112))

### Documentation

* Add and improve documentation (Bodigrim, konsumlamm, Toni Dietze, alexfmpe,
  Soumik Sarkar, Jonathan Knowles, Xavier Góngora, Xia Li-yao, eyelash)
  ([#957](https://github.com/haskell/containers/pull/957),
  [#1006](https://github.com/haskell/containers/pull/1006),
  [#877](https://github.com/haskell/containers/pull/877),
  [#960](https://github.com/haskell/containers/pull/960),
  [#1033](https://github.com/haskell/containers/pull/1033),
  [#1041](https://github.com/haskell/containers/pull/1041),
  [#1039](https://github.com/haskell/containers/pull/1039),
  [#1050](https://github.com/haskell/containers/pull/1050),
  [#1088](https://github.com/haskell/containers/pull/1088),
  [#1087](https://github.com/haskell/containers/pull/1087),
  [#1098](https://github.com/haskell/containers/pull/1098),
  [#1106](https://github.com/haskell/containers/pull/1106),
  [#1104](https://github.com/haskell/containers/pull/1104),
  [#1105](https://github.com/haskell/containers/pull/1105),
  [#1111](https://github.com/haskell/containers/pull/1111),
  [#1110](https://github.com/haskell/containers/pull/1110),
  [#1114](https://github.com/haskell/containers/pull/1114),
  [#1115](https://github.com/haskell/containers/pull/1115))

### Miscellaneous/internal

* Internal modules `Utils.Containers.Internal.BitUtil`,
  `Utils.Containers.Internal.BitQueue`, `Utils.Containers.Internal.StrictPair`
  are no longer exposed. (Soumik Sarkar)
  ([#1101](https://github.com/haskell/containers/pull/1101))

* Test and CI maintenance. (Andreas Abel, Soumik Sarkar)
  ([#986](https://github.com/haskell/containers/pull/986),
  [#1015](https://github.com/haskell/containers/pull/1015),
  [#1030](https://github.com/haskell/containers/pull/1030),
  [#1055](https://github.com/haskell/containers/pull/1055),
  [#1067](https://github.com/haskell/containers/pull/1067))

* Internal cleanups and improvements. (Soumik Sarkar, alexfmpe)
  ([#1000](https://github.com/haskell/containers/pull/1000),
  [#959](https://github.com/haskell/containers/pull/959),
  [#1020](https://github.com/haskell/containers/pull/1020),
  [#1029](https://github.com/haskell/containers/pull/1029),
  [#1031](https://github.com/haskell/containers/pull/1031),
  [#1037](https://github.com/haskell/containers/pull/1037),
  [#1058](https://github.com/haskell/containers/pull/1058),
  [#1076](https://github.com/haskell/containers/pull/1076),
  [#1084](https://github.com/haskell/containers/pull/1084),
  [#1085](https://github.com/haskell/containers/pull/1085),
  [#1093](https://github.com/haskell/containers/pull/1093),
  [#1094](https://github.com/haskell/containers/pull/1094),
  [#1095](https://github.com/haskell/containers/pull/1095),
  [#1097](https://github.com/haskell/containers/pull/1097),
  [#1103](https://github.com/haskell/containers/pull/1103),
  [#1117](https://github.com/haskell/containers/pull/1117))

* Add new tests and benchmarks (Soumik Sarkar)
  ([#962](https://github.com/haskell/containers/pull/962),
  [#1021](https://github.com/haskell/containers/pull/1021),
  [#1063](https://github.com/haskell/containers/pull/1063),
  [#1068](https://github.com/haskell/containers/pull/1068),
  [#1071](https://github.com/haskell/containers/pull/1071),
  [#1075](https://github.com/haskell/containers/pull/1075),
  [#1082](https://github.com/haskell/containers/pull/1082))

* Fix the Read the Docs tutorial (Soumik Sarkar)
  ([#1091](https://github.com/haskell/containers/pull/1091),
  [#1099](https://github.com/haskell/containers/pull/1099))

## 0.7

### Breaking changes

* Breaking changes to `Data.Graph.SCC v` (bodʲɪˈɡrʲim):
  * `CyclicSCC [v]` is now not a constructor,
    but a bundled pattern synonym for backward compatibility.
  * `NECyclicSCC (NonEmpty v)` is a new constructor, maintaining an invariant
    that a set of mutually reachable vertices is non-empty.

## 0.6.8

### Additions

* Add `Data.IntSet.fromRange`. (Soumik Sarkar)

### Improvements

* Speed up conversion from monotonic lists to `Set`s and
  `Map`s. (Soumik Sarkar)

### Documentation and other

* Add, improve, and correct documentation. (Niklas Hambüchen, Soumik Sarkar,
  tomjaguarpaw, Alice Rixte, Tom Smeding)

### Other/internal

* Remove the `stack.yaml` file. It was extremely stale, and its utility was a
  bit dubious in a GHC boot package. Closes #938.

* Add a bunch of new tests and benchmarks. (Soumik Sarkar)

* Future-proof test suite against export of `foldl'` from `Prelude`.
  (Teo Camarasu)

## 0.6.7

### Additions

* Add `takeWhileAntitone`, `dropWhileAntitone`, and `spanAntitone` for `IntMap`
  and `IntSet`. (Soumik Sarkar)

* Add a `Foldable1` instance for `Data.Tree`.

### Improvements

* Speed up splitting functions for `IntSet` and `IntMap`. (Soumik Sarkar)

* Speed up `Foldable` methods for `Data.Tree`. (Soumik Sarkar)

* Speed up `Data.Graph.dfs` (Soumik Sarkar)

* Inline a few functions in `Data.Graph` to enable list fusion. This
  immediately improves the performance of the `transposeG` and `scc` functions.
  Mark several others `INLINABLE` to allow specialization.  (Soumik Sarkar)

* Optimize `Data.Graph.bcc`, most notably replacing lists by difference lists
  to avoid quadratic complexity. (Soumik Sarkar)

### Documentation

* Improve various documentation and documentation formatting (Joseph C. Sible,
  konsumlamm, Soumik Sarkar, Alberto Fanton)

* Add and correct time complexity documentation. (Soumik Sarkar)

* Update `CONTRIBUTING.md` instructions for building with `stack` and `cabal`,
  and add a note about how to avoid unnecessary recompilations. (Melanie
  Phoenix)

### Miscellaneous/internal

* Remove now-redundant CPP. (Alexandre Esteves)
* Avoid `head` and `tail`. (Bodigrim)
* Fix build paths in `gitignore`. (Alexandre Esteves)
* Add extra implicit dependencies for `DeriveLift`. (Matthew Pickering)
* Work around `Prelude` changes for `liftA2`. (David Feuer)
* Add several property tests and too many benchmarks to count. (Soumik Sarkar)
* Add benchmarks for `Data.Set.powerSet`. (jwaldmann)
* Improve `Data.Set.powerSet` property test. (David Feuer)
* Fix test name. (Marcin Szamotulski)
* Fix error messages in internal `Data.Set` functions. (Erik de Castro Lopo)

## 0.6.6

### Additions

* Add `Lift` instances for use with Template Haskell. Specifically:
  `Seq`, `ViewL`, and `ViewR` (in `Data.Sequence`), `Map`, `Set`,
  `IntMap`, `IntSet`, `Tree`, and `SCC` (in `Data.Graph`). (David Feuer)

* Add `argSet` and `fromArgSet` for `Data.Map`. (Joseph C. Sible)

### Performance improvements

* Remove short-circuiting from certain `IntMap` functions to improve
  performance for successful lookups. (Callan McGill)

### Other changes

* Drop support for GHC versions before 8.0.2. (David Feuer)

* Various documentation improvements. (Will Hawkins, Eric Lindblad, konsumlamm,
  Joseph C. Sible)

### Miscellaneous/internal

* Bump Cabal version for tests, and use `common` clauses to reduce
  duplication. (David Feuer)

* Migrate from test-framework to tasty. (Bodigrim)

* Migrate from gauge to tasty-bench. (Bodigrim)

* Enable `TypeOperators` to address a future GHC requirement.
  (Vladislav Zavialov)

* Work around an issue with unboxed arrays on big-endian systems.
  (Peter Trommler)

## 0.6.5.1

### Bug fixes

* `foldr'` and `foldl'` for `Map` and `Set` are now strict everywhere they
  should be, and we have detailed tests to make sure they stay that way.
  (Thanks, coot.)

* The `Ord IntSet` instance, which was broken in 0.6.3.1, has been
  repaired.

### New instance

* We now have `Ord a => Ord (Tree a)` (Thanks, Ericson2314.)

### Testing fixes

* Thanks to konsumlamm and infinity0 for various bug fixes in the test suite.

## 0.6.4.1

### Bug fixes

* [Replace value-forcing variants of `compose` with lazy variants.](https://github.com/haskell/containers/pull/745)
  *  This brings `compose` closer in line with functions like `union` and `intersection` which don't evaluate any map values. (Thanks, Simon Jakobi)

### Additions

* [Add `reverseTopSort` to `Data.Graph`](https://github.com/haskell/containers/pull/638) (Thanks, James Parker)

* [Expose `traverseMaybeWithKey` from `Data.IntMap.{Lazy,Strict}`](https://github.com/haskell/containers/pull/743) (Thanks, Simon
  Jakobi)

### Other changes

* Improvements to the testsuite: [#663](https://github.com/haskell/containers/pull/663), [#662](https://github.com/haskell/containers/pull/662) (Thanks, Bertram Felgenhauer)

* [Fix build with `stack test`](https://github.com/haskell/containers/pull/738) (Thanks, Simon Jakobi)

[0.6.4.1]: https://github.com/haskell/containers/compare/v0.6.3.1-release...v0.6.4.1

## 0.6.3.1

### Bug fixes

* Fix `traverse` and `traverseWithKey` for `IntMap`, which would
  previously produce invalid `IntMap`s when the input contained
  negative keys (Thanks, Felix Paulusma).

* Fix the traversal order of various functions for `Data.IntMap`:
  `traverseWithKey`, `traverseMaybeWithKey`, `filterWithKeyA`,
  `minimum`, `maximum`, `mapAccum`, `mapAccumWithKey`, `mapAccumL`,
  `mapAccumRWithKey`, `mergeA` (Thanks, Felix Paulusma, Simon Jakobi).

### Additions

* Add `compose` for `Map` and `IntMap` (Thanks, Alexandre Esteves).

* Add `alterF` for `Set` and `IntSet` (Thanks, Simon Jakobi).

* Add `Data.IntSet.mapMonotonic` (Thanks, Javran Cheng).

* Add `instance Bifoldable Map` (Thanks, Joseph C. Sible).

### Performance improvements

* Make `(<*)` for `Data.Sequence` incrementally asymptotically optimal.
  This finally completes the task, begun in December 2014, of making all
  the `Applicative` methods for sequences asymptotically optimal
  even when their results are consumed incrementally. Many thanks to
  Li-Yao Xia and Bertram Felgenhauer for helping to clean up and begin
  to document this rather tricky code.

* Speed up `fromList` and related functions in `Data.IntSet`, `Data.IntMap`
  and `Data.IntMap.Strict` (Thanks, Bertram Felgenhauer).

* Use `count{Leading,Trailing}Zeros` in `Data.IntSet` internals (Thanks, Alex
  Biehl).

### Other changes

* Reduce usage of the `Forest` type synonym in `Data.Tree` (Thanks, David
  Feuer).

* Address a Core lint warning for `foldToMaybeTree` (Thanks, Matthew Pickering).

* Improve documentation (Thanks to Daniel Wagner, Johannes Waldmann, Steve Mao,
  Gabriel Greif, Jean-Baptiste Mazon, Ziyang Liu, Matt Renaud, Li-Yao Xia).

* Improvements to the testsuite and benchmarks (Thanks, Bertram Felgenhauer,
  Simon Jakobi, Johannes Waldmann).

* Canonicalise `Seq`'s `Monoid` instance (Thanks, Fumiaki Kinoshita).

## 0.6.2.1

* Add `disjoint` for `Map` and `IntMap` (Thanks, Simon Jakobi).

* Fix documentation bugs (Thanks, olligobber).

* Fix unused imports (Thanks, Ben Gamari).

## 0.6.1.1

* Fix Foldable instance for IntMap, which previously placed positively
  keyed entries before negatively keyed ones for `fold`, `foldMap`, and
  `traverse`.

* Make strict `IntMap` merges strict.

* Make `Data.IntMap.Merge.Strict` tactics (except `preserveMissing`)
  strict.

* Add a strict `Data.Map.Merge.Strict.preserveMissing'` tactic.

* Make `stimes` for sequences work with 0 arguments, and make it more
  efficient.

* Speed up `cartesianProduct` for `Data.Set`.

* Speed up `Data.Set.isSubsetOf`, `Data.Map.isSubmapOf`, and `Data.Set.disjoint`.

* Allow inlining for `Data.Sequence.traverseWithIndex`, making it faster
  than `sequence` combined with `mapWithIndex`.

* Produce more concise assembly from `maskW`. (Thanks, Mateusz Kowalczyk)

* Use `countLeadingZeros` to implement `highestBitMask` (Thanks, Dmitry
  Ivanov)

* Improve documentation. (Thanks to jwaldmann, Yuji Yamamoto, David Sanders,
  Alec Theriault, Vaibhav Sagar, Boro Sitnikovski, Morten Kolstad, Vados,
  Benjamin Web, Chris Martin, Alexandre Esteves).

* Clean up packaging and testing. (Thanks, David Eichmann, Simon Jakobi,
  Oleg Grenrus, Andreas Klebinger)

## 0.6.0.1

* Released with GHC 8.6

### New functions and class instances

* Add `Data.Containers.ListUtils` offering `nub`-like functions. (Thanks to
  Gershom Bazerman for starting the process of writing these.)

* Add `Generic` and `Generic1` instances for key `Data.Sequence.Internal` types:
  `Node`, `Digit`, `Elem`, and `FingerTree`.

### Death of deprecated functions

The following functions have been disabled. As an experiment
in function removal technology, the functions are still temporarily present,
but any attempts to use them will result in type errors advising on
replacement.

* `Data.IntMap`: `insertWith'`, `insertWithKey'`, `fold`, and `foldWithKey`.

* `Data.Map`: `insertWith'`, `insertWithKey'`, `insertLookupWithKey'`,
   `fold`, and `foldWithKey`.

The same has been done for the deprecated exports of `showTree` and
`showTreeWith`. These function remain available in the internal `Debug`
modules.

### Changes to existing functions

* Generalize the types of `unions` and `unionsWith`. (Thanks, jwaldmann.)

### Performance improvements

* Speed up folds and `traverse` on sequences. (Thanks, Donnacha Oisín Kidney.)

* Speed up `Data.Set.union`. (Thanks, Joachim Breitner.)

* Speed up several algorithms in `Data.Graph` a bit by using unboxed arrays
  where appropriate.

* Implement `Data.Graph.indegree` directly instead of taking the transpose
  and calculating its `outdegree`. This may not lead to an immediate performance
  improvement (see [GHC Trac #14785](https://ghc.haskell.org/trac/ghc/ticket/14785))
  but it should be better eventually.

### Other package changes

* Drop support for GHC versions before 7.6.

* Improve `Data.Graph` documentation, and reorganize map and set documentation.

* Remove the `Data.Map.Lazy.Merge` and `Data.Map.Strict.Merge` modules. These
  were renamed and deprecated almost as soon as they were introduced.


## 0.5.11

* Released with GHC 8.4.

### New functions and class instances

* Add a `MonadFix` instance for `Data.Sequence`.

* Add a `MonadFix` instance for `Data.Tree`.

* Add `powerSet`, `cartesianProduct`, and `disjointUnion` for
  `Data.Set`. (Thanks, Edward Kmett.)

* Add `disjoint` for `Data.Set` and `Data.IntSet`. (Thanks, Víctor López Juan.)

* Add `lookupMin` and `lookupMax` to `Data.IntMap`. (Thanks, bwroga.)

* Add `unzip` and `unzipWith` to `Data.Sequence`. Make unzipping
  build its results in lockstep to avoid certain space leaks.

* Add carefully optimized implementations of `sortOn` and `unstableSortOn`
  to `Data.Sequence`. (Thanks, Donnacha Oisín Kidney.)

### Changes to existing functions and features

* Make `Data.Sequence.replicateM` a synonym for `replicateA`
  for post-AMP `base`.

* Rewrite the `IsString` instance head for sequences, improving compatibility
  with the list instance and also improving type inference. We used to have

  ```haskell
  instance IsString (Seq Char)
  ```

  Now we commit more eagerly with

  ```haskell
  instance a ~ Char => IsString (Seq a)
  ```

* Make `>>=` for `Data.Tree` strict in the result of its second argument;
  being too lazy here is almost useless, and violates one of the monad identity
  laws. Specifically, `return () >>= \_ -> undefined` should always be
  `undefined`, but this was not the case.

* Harmonize laziness details for `minView` and `maxView` between
  `Data.IntMap` and `Data.Map`.

### Performance improvement

* Speed up both stable and unstable sorting for `Data.Sequence` by (Thanks, Donnacha
  Oisín Kidney.)

### Other changes

* Update for recent and upcoming GHC and Cabal versions (Thanks, Herbert
  Valerio Reidel, Simon Jakobi, and Ryan Scott.)

* Improve external and internal documentation (Thanks, Oleg Grenrus
  and Benjamin Hodgson.)

* Add tutorial-style documentation.

* Add Haddock `@since` annotations for changes made since version
  0.5.4 (Thanks, Simon Jakobi.)

* Add a (very incomplete) test suite for `Data.Tree`.

* Add structural validity checks to the test suites for `Data.IntMap`
  and `Data.IntSet` (Thanks to Joachim Breitner for catching an error
  in a first draft.)

## 0.5.10.2

* Released with GHC 8.2.

* Use `COMPLETE` pragmas to declare complete sets of pattern synonyms
  for `Data.Sequence`. At last!

* Make `Data.IntMap.Strict.traverseWithKey` force the values before
  installing them in the result. Previously, this function could be used to
  produce an `IntMap` containing undefined values.

* Fix strictness bugs in various rewrite rules for `Data.Map.Strict` and
  `Data.IntMap.Strict`. Previously, rules could unintentionally reduce
  strictness. The most important change in this regard is the elimination
  of rules rewriting `*.Strict.map coerce` to `coerce`. To map a coercion
  over a structure for free, be sure to use the lazy `map` or `fmap`.
  It is possible to write rules that do a somewhat better job of this, but
  it turns out to be a bit messy.

* Optimize `Data.IntMap.restrictKeys` and `Data.IntMap.withoutKeys`. The
  semantic fix in 0.5.10.1 left them rather slow in certain cases.

* Speed up `size` for `IntSet` and `IntMap` (thanks, Mike Ledger!).

* Define a custom `liftA2` in `Applicative` instances for base 4.10, and use
  `liftA2` rather than `<*>` whenever it may be beneficial.

* Add `liftA2`-related `RULES` for `Data.Sequence`.

* Export non-deprecated versions of `showTree` and `showTreeWith` from
  `Data.IntMap.Internal.Debug`.

## 0.5.10.1

* Fix completely incorrect implementations of `Data.IntMap.restrictKeys` and
  `Data.IntMap.withoutKeys`. Make the tests for these actually run. (Thanks
  to Tom Smalley for reporting this.)

* Fix a minor bug in the `Show1` instance of `Data.Tree`. This produced valid
  output, but with fewer parentheses than `Show`. (Thanks, Ryan Scott.)

* Add `MonadZip` instance for `Data.Sequence`.

* Remove meaningless stability annotations (Thanks, Simon Jakobi.)

## 0.5.9.2

* Backport bug fixes from 0.5.10.1

## 0.5.9.1

* Add `merge` and `mergeA` for `Data.IntMap`.

* Add instances for `Data.Graph.SCC`: `Foldable`, `Traversable`, `Data`,
  `Generic`, `Generic1`, `Eq`, `Eq1`, `Show`, `Show1`, `Read`, and `Read1`.

* Add lifted instances (from `Data.Functor.Classes`) for `Data.Sequence`,
  `Data.Map`, `Data.Set`, `Data.IntMap`, and `Data.Tree`. (Thanks to
  Oleg Grenrus for doing a lot of this work.)

* Properly deprecate functions in `Data.IntMap` long documented as deprecated.

* Rename several internal modules for clarity. Thanks to esoeylemez for starting
  this process.

* Make `Data.Map.fromDistinctAscList` and `Data.Map.fromDistinctDescList` more
  eager, improving performance.

* Plug space leaks in `Data.Map.Lazy.fromAscList` and
 `Data.Map.Lazy.fromDescList` by manually inlining constant functions.

* Add `lookupMin` and `lookupMax` to `Data.Set` and `Data.Map` as total
alternatives to `findMin` and `findMax`.

* Add `!?` to `Data.Map` as a total alternative to `!`.

* Avoid using `deleteFindMin` and `deleteFindMax` internally, preferring
total functions instead. New implementations of said functions lead to slight
performance improvements overall.

## 0.5.8.2

* Backport bug fixes from 0.5.10.1.

## 0.5.8.1 *Aug 2016*

### General package changes

  * Remove all attempts to support nhc98 and any versions of GHC
    before 7.0.

  * Integrate benchmarks with Cabal. (Thanks, Gabriel Gonzalez!)

  * Make Cabal report required extensions properly, and stop using
    default extensions. Note that we do *not* report extensions conditionally enabled
    based on GHC version, as doing so would lead to a maintenance nightmare
    with no obvious benefits.

  * Use `BangPatterns` throughout to reduce noise. This extension
    is now *required* to compile `containers`.

  * Improve QuickCheck properties taking arbitrary functions by using
    `Test.QuickCheck.Function.Fun` instead of evil `Show` instances
    for functions.

  * Expose several internal modules through Cabal (as requested by
    Edward Kmett). These remain completely unsupported.

### New exports and instances

  * Add `alterF`, `restrictKeys`, and `withoutKeys` to `Data.Map`
    and `Data.IntMap`.

  * Add `take`, `drop`, `splitAt`, `takeWhileAntitone`, `dropWhileAntitone`,
    and `spanAntitone` for `Data.Map` and `Data.Set`. Thanks to Cale Gibbard
    for suggesting these.

  * Add `merge`, `mergeA`, and associated merge tactics for `Data.Map`.
    Many thanks to Cale Gibbard, Ryan Trinkle, and Dan Doel for
    inspiring the merge idea and helping refine the interface.

  * Add `traverseMaybeWithKey`, `fromDescList`, `fromDescListWith`,
    `fromDescListWithKey`, and `fromDistinctDescList` to `Data.Map`.

  * Add `fromDescList` and `fromDistinctDescList` to `Data.Set`.

  * Add `Empty`, `:<|`, and `:|>` pattern synonyms for `Data.Sequence`.

  * Add `adjust'`, `(!?)`, `lookup`, `chunksOf`, `cycleTaking`, `insertAt`, `deleteAt`, `intersperse`,
    `foldMapWithIndex`, and `traverseWithIndex` for `Data.Sequence`.

  * Derive `Generic` and `Generic1` for `Data.Tree.Tree`, `Data.Sequence.ViewL`,
    and `Data.Sequence.ViewR`.

  * Add `foldTree` for `Data.Tree`. (Thanks, Daniel Wagner!)

### Semantic changes

  * Make `Data.Sequence.splitAt` strict in its arguments. Previously,
    it returned a lazy pair.

  * Fix completely erroneous definition of `length` for `Data.Sequence.ViewR`.

  * Make `Data.Map.Strict.traverseWithKey` force result values before
    installing them in the new map.

  * Make `drawTree` handle newlines better. (Thanks, recursion-ninja!)

### Deprecations

  * All functions in `Data.Map` proper that have been documented as deprecated since
    version 0.5 or before now have `DEPRECATED` pragmas and will actually be
    removed after another cycle or two.

  * Tree printing functions in `Data.Map` intended for library debugging are now
    deprecated. They will continue to be available for the foreseeable future in
    an internal module.

### Performance changes

  * Substantially speed up `splitAt`, `zipWith`, `take`, `drop`,
    `fromList`, `partition`, `foldl'`, and `foldr'` for `Data.Sequence`.
    Special thanks to Lennart Spitzner for digging into the performance
    problems with previous versions of `fromList` and finding a way to
    make it really fast. Slightly optimize `replicateA`. Stop `traverse`
    from performing many unnecessary `fmap` operations.

  * Most operations in `Data.Sequence` advertised as taking logarithmic
    time (including `><` and `adjust`) now use their full allotted time
    to avoid potentially building up chains of thunks in the tree. In general,
    the only remaining operations that avoid doing more than they
    really need are the particular bulk creation and transformation functions
    that really benefit from the extra laziness. There are some situations
    where this change may slow programs down, but I think having more
    predictable and usually better performance more than makes up for that.

  * Add rewrite rules to fuse `fmap` with `reverse` for `Data.Sequence`.

  * Switch from *hedge* algorithms to *divide-and-conquer* algorithms
    for union, intersection, difference, and merge in both `Data.Map`
    and `Data.Set`. These algorithms are simpler, are known to be
    asymptotically optimal, and are faster according to our benchmarks.

  * Speed up `adjust` for `Data.Map`. Allow `map` to inline, and
    define a custom `(<$)`. This considerably improves mapping with
    a constant function.

  * Remove non-essential laziness in `Data.Map.Lazy` implementation.

  * Speed up deletion and alteration functions for `Data.IntMap`.


## 0.5.7.1  *Dec 2015*

  * Planned to bundle with GHC 8.0.1.

  * Add `IsString` instance to `Data.Sequence`.

  * Define `Semigroup` instances for `Data.Map`, `Data.Set`, `Data.IntMap`,
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
