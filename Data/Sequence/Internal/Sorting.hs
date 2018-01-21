{-# LANGUAGE BangPatterns #-}


------------------------------------------------------------------------
-- |
-- Sorting
--
-- Unstable sorting is performed by a heap sort implementation based on
-- pairing heaps.  Because the internal structure of sequences is quite
-- varied, it is difficult to get blocks of elements of roughly the same
-- length, which would improve merge sort performance.  Pairing heaps,
-- on the other hand, are relatively resistant to the effects of merging
-- heaps of wildly different sizes, as guaranteed by its amortized
-- constant-time merge operation.  Moreover, extensive use of SpecConstr
-- transformations can be done on pairing heaps, especially when we're
-- only constructing them to immediately be unrolled.
--
-- On purely random sequences of length 50000, with no RTS options,
-- I get the following statistics, in which heapsort is about 42.5%
-- faster:  (all comparisons done with -O2)
--
-- Times (ms)            min      mean    +/-sd    median    max
-- to/from list:       103.802  108.572    7.487  106.436  143.339
-- unstable heapsort:   60.686   62.968    4.275   61.187   79.151
--
-- Heapsort, it would seem, is less of a memory hog than Data.List.sortBy.
-- The gap is narrowed when more memory is available, but heapsort still
-- wins, 15% faster, with +RTS -H128m:
--
-- Times (ms)            min    mean    +/-sd  median    max
-- to/from list:       42.692  45.074   2.596  44.600  56.601
-- unstable heapsort:  37.100  38.344   3.043  37.715  55.526
--
-- In addition, on strictly increasing sequences the gap is even wider
-- than normal; heapsort is 68.5% faster with no RTS options:
-- Times (ms)            min    mean    +/-sd  median    max
-- to/from list:       52.236  53.574   1.987  53.034  62.098
-- unstable heapsort:  16.433  16.919   0.931  16.681  21.622
--
-- This may be attributed to the elegant nature of the pairing heap.
--
-- wasserman.louis@gmail.com, 7/20/09
------------------------------------------------------------------------
-- David Feuer wrote an unstable sort for arbitrary traversables,
-- https://www.reddit.com/r/haskell/comments/63a4ea/fast_total_sorting_of_arbitrary_traversable/,
-- which turned out to be competitive with the unstable sort here.
-- Feuer suggested that this indicated some room to improve on the
-- unstable sort.
--
-- The two main improvements to the original function are a specialize
-- pragma on replicateA (this gives a 26.5% speedup) and removal of the
-- intermediate list (a further 11.7% speedup). These numbers are all on
-- purely random sequences of length 50000:
--
-- Times (ms)            min    est    max  std dev   R²
-- to/from list:        70.90  72.44  75.07  2.224  0.998
-- 7/20/09 heapsort:    59.84  61.44  63.08  1.554  0.998
-- 7/20/09 w/pragma:    44.22  45.14  46.25  1.631  0.998
-- 4/30/17 heapsort:    38.21  39.86  40.87  1.203  0.996
--
-- It should also be noted that Data.List.sortBy has become
-- significantly quicker. Data.List.sortBy also now recognizes strictly
-- increasing sequences, making it much quicker for that case:
--
-- Times (ms)            min    est    max  std dev   R²
-- to/from list:        7.140  7.351  7.634  0.335  0.993
-- 7/20/09 heapsort:    19.52  19.78  20.13  0.445  0.999
-- 7/20/09 w/pragma:    8.050  8.271  8.514  0.357  0.995
-- 4/30/17 heapsort:    7.240  7.612  7.925  0.389  0.991
--
-- Another happy result of the specialization of 'replicateA' is that
-- the stable sort seems to speed up by 10-20%, and 'iterateN' looks
-- like it's about three times as fast.
--
-- mail@doisinkidney.com, 4/30/17
------------------------------------------------------------------------
-- The sort and sortBy functions are implemented by tagging each element
-- in the input sequence with its position, and using that to
-- discriminate between elements which are equivalent according to the
-- comparator. This makes the sort stable.
--
-- The algorithm is effectively the same as the unstable sorts, except
-- the queue is constructed while giving each element a tag.
--
-- It's quicker than the old implementation (which used Data.List.sort)
-- in the general case (all times are on sequences of length 50000):
--
-- Times (ms)            min    est    max  std dev   r²
-- to/from list:        64.23  64.50  64.81  0.432  1.000
-- 1/11/18 stable heap: 38.87  39.40  40.09  0.457  0.999
--
-- Slightly slower in the case of already sorted lists:
--
-- Times (ms)            min    est    max  std dev   r²
-- to/from list:        6.806  6.861  6.901  0.234  1.000
-- 1/11/18 stable heap: 8.211  8.268  8.328  0.111  1.000
--
-- And quicker in the case of lists sorted in reverse:
--
-- Times (ms)            min    est    max  std dev   r²
-- to/from list:        26.79  28.34  30.55  1.219  0.988
-- 1/11/18 stable heap: 9.405  10.13  10.91  0.670  0.977
--
-- Interestingly, the stable sort is now competitive with the unstable:
--
-- Times (ms)            min    est    max  std dev   r²
-- unstable:            34.71  35.10  35.38  0.845  1.000
-- stable:              38.84  39.22  39.59  0.564  0.999
--
-- And even beats it in the case of already-sorted lists:
--
-- Times (ms)            min    est    max  std dev   r²
-- unstable:            8.457  8.499  8.536  0.069  1.000
-- stable:              8.160  8.230  8.334  0.158  0.999
--
-- mail@doisinkidney.com, 1/11/18
------------------------------------------------------------------------
-- Further notes are available in the file sorting.md (in this
-- directory).
------------------------------------------------------------------------

module Data.Sequence.Internal.Sorting where

import Data.Sequence.Internal

-- | \( O(n \log n) \).  'sort' sorts the specified 'Seq' by the natural
-- ordering of its elements.  The sort is stable.  If stability is not
-- required, 'unstableSort' can be slightly faster.
sort :: Ord a => Seq a -> Seq a
sort = sortBy compare

-- | \( O(n \log n) \).  'sortBy' sorts the specified 'Seq' according to the
-- specified comparator.  The sort is stable.  If stability is not required,
-- 'unstableSortBy' can be slightly faster.
sortBy :: (a -> a -> Ordering) -> Seq a -> Seq a
sortBy cmp (Seq xs) =
    maybe
        (Seq EmptyT)
        (execState (replicateA (size xs) (popMinS cmp)))
        (toPQS cmp (\s (Elem x) -> PQS s x Nl) 0 xs)

-- | \( O(n \log n) \). 'sortOn' sorts the specified 'Seq' by comparing
-- the results of a key function applied to each element. @'sortOn' f@ is
-- equivalent to @'sortBy' ('compare' ``Data.Function.on`` f)@, but has the
-- performance advantage of only evaluating @f@ once for each element in the
-- input list. This is called the decorate-sort-undecorate paradigm, or
-- Schwartzian transform.
--
-- An example of using 'sortOn' might be to sort a 'Seq' of strings
-- according to their length:
--
-- > sortOn length (fromList ["alligator", "monkey", "zebra"]) == fromList ["zebra", "monkey", "alligator"]
--
-- If, instead, 'sortBy' had been used, 'length' would be evaluated on
-- every comparison, giving \( O(n \log n) \) evaluations, rather than
-- \( O(n) \).
--
-- If @f@ is very cheap (for example a record selector, or 'fst'),
-- @'sortBy' ('compare' ``Data.Function.on`` f)@ will be faster than
-- @'sortOn' f@.
sortOn :: Ord b => (a -> b) -> Seq a -> Seq a
sortOn f (Seq xs) =
    maybe
       (Seq EmptyT)
       (execState (replicateA (size xs) (popMinST compare)))
       (toPQST compare (\s (Elem x) -> PQST s (f x) x NlT) 0 xs)

-- | \( O(n \log n) \).  'unstableSort' sorts the specified 'Seq' by
-- the natural ordering of its elements, but the sort is not stable.
-- This algorithm is frequently faster and uses less memory than 'sort'.

-- Notes on the implementation and choice of heap are available in
-- the file sorting.md (in this directory).
unstableSort :: Ord a => Seq a -> Seq a
unstableSort = unstableSortBy compare

-- | \( O(n \log n) \).  A generalization of 'unstableSort', 'unstableSortBy'
-- takes an arbitrary comparator and sorts the specified sequence.
-- The sort is not stable.  This algorithm is frequently faster and
-- uses less memory than 'sortBy'.
unstableSortBy :: (a -> a -> Ordering) -> Seq a -> Seq a
unstableSortBy cmp (Seq xs) =
    maybe
        (Seq EmptyT)
        (execState (replicateA (size xs) (popMin cmp)))
        (toPQ cmp (\(Elem x) -> PQueue x Nil) xs)

-- | \( O(n \log n) \). 'unstableSortOn' sorts the specified 'Seq' by
-- comparing the results of a key function applied to each element.
-- @'unstableSortOn' f@ is equivalent to @'unstableSortBy' ('compare' ``Data.Function.on`` f)@,
-- but has the performance advantage of only evaluating @f@ once for each
-- element in the input list. This is called the
-- decorate-sort-undecorate paradigm, or Schwartzian transform.
--
-- An example of using 'unstableSortOn' might be to sort a 'Seq' of strings
-- according to their length:
--
-- > unstableSortOn length (fromList ["alligator", "monkey", "zebra"]) == fromList ["zebra", "monkey", "alligator"]
--
-- If, instead, 'unstableSortBy' had been used, 'length' would be evaluated on
-- every comparison, giving \( O(n \log n) \) evaluations, rather than
-- \( O(n) \).
--
-- If @f@ is very cheap (for example a record selector, or 'fst'),
-- @'unstableSortBy' ('compare' ``Data.Function.on`` f)@ will be faster than
-- @'unstableSortOn' f@.
unstableSortOn :: Ord b => (a -> b) -> Seq a -> Seq a
unstableSortOn f (Seq xs) =
    maybe
       (Seq EmptyT)
       (execState (replicateA (size xs) (popMinT compare)))
       (toPQT compare (\(Elem x) -> PQT (f x) x NilT) xs)

-- | A 'PQueue' is a simple pairing heap.
data PQueue e = PQueue e (PQL e)
data PQL e = Nil | {-# UNPACK #-} !(PQueue e) :& PQL e

infixr 8 :&

-- | 'popMin', given an ordering function, constructs a stateful action
-- which pops the smallest elements from a queue. This action will fail
-- on empty queues.
popMin :: (e -> e -> Ordering) -> State (PQueue e) e
popMin cmp = State unrollPQ'
  where
    {-# INLINE unrollPQ' #-}
    unrollPQ' (PQueue x ts) = (mergePQs ts, x)
    mergePQs (t :& Nil) = t
    mergePQs (t1 :& t2 :& Nil) = t1 <+> t2
    mergePQs (t1 :& t2 :& ts) = (t1 <+> t2) <+> mergePQs ts
    mergePQs Nil = error "popMin: tried to pop from empty queue"
    (<+>) = mergePQ cmp

{-# INLINE foldToMaybeTree #-}
foldToMaybeTree :: (b -> b -> b) -> (a -> b) -> FingerTree a -> Maybe b
foldToMaybeTree _ _ EmptyT = Nothing
foldToMaybeTree _ f (Single xs) = Just (f xs)
foldToMaybeTree (<+>) f (Deep _ pr m sf) =
    Just (maybe (pr' <+> sf') ((pr' <+> sf') <+>) m')
  where
    pr' = foldDigit (<+>) f pr
    sf' = foldDigit (<+>) f sf
    m' = foldToMaybeTree (<+>) (foldNode (<+>) f) m

-- | 'toPQ', given an ordering function and a mechanism for queueifying
-- elements, converts a 'Seq' to a 'PQueue'.
toPQ :: (b -> b -> Ordering) -> (a -> PQueue b) -> FingerTree a -> Maybe (PQueue b)
toPQ cmp = foldToMaybeTree (mergePQ cmp)

-- | 'mergePQ' merges two 'PQueue's.
mergePQ :: (a -> a -> Ordering) -> PQueue a -> PQueue a -> PQueue a
mergePQ cmp q1@(PQueue x1 ts1) q2@(PQueue x2 ts2)
  | cmp x1 x2 == GT     = PQueue x2 (q1 :& ts2)
  | otherwise           = PQueue x1 (q2 :& ts1)

-- | A pairing heap tagged with the original position of elements,
-- to allow for stable sorting.
data PQS e = PQS {-# UNPACK #-} !Int e (PQSL e)
data PQSL e = Nl | {-# UNPACK #-} !(PQS e) :&& PQSL e

infixr 8 :&&

-- | 'popMinS', given an ordering function, constructs a stateful action
-- which pops the smallest elements from a queue. This action will fail
-- on empty queues.
popMinS :: (e -> e -> Ordering) -> State (PQS e) e
popMinS cmp = State unrollPQ'
  where
    {-# INLINE unrollPQ' #-}
    unrollPQ' (PQS _ x ts) = (mergePQs ts, x)
    mergePQs (t :&& Nl) = t
    mergePQs (t1 :&& t2 :&& Nl) = t1 <+> t2
    mergePQs (t1 :&& t2 :&& ts) = (t1 <+> t2) <+> mergePQs ts
    mergePQs Nl = error "popMin: tried to pop from empty queue"
    (<+>) = mergePQS cmp

foldToMaybeWithIndexTree :: (b -> b -> b) -> (Int -> Elem y -> b) -> Int -> FingerTree (Elem y) -> Maybe b
foldToMaybeWithIndexTree = foldToMaybeWithIndexTree'
  where
    {-# SPECIALISE foldToMaybeWithIndexTree' :: (b -> b -> b) -> (Int -> Elem y -> b) -> Int -> FingerTree (Elem y) -> Maybe b #-}
    {-# SPECIALISE foldToMaybeWithIndexTree' :: (b -> b -> b) -> (Int -> Node y -> b) -> Int -> FingerTree (Node y) -> Maybe b #-}
    foldToMaybeWithIndexTree' :: Sized a => (b -> b -> b) -> (Int -> a -> b) -> Int -> FingerTree a -> Maybe b
    foldToMaybeWithIndexTree' _ _ !_s EmptyT = Nothing
    foldToMaybeWithIndexTree' _ f s (Single xs) = Just (f s xs)
    foldToMaybeWithIndexTree' (<+>) f s (Deep _ pr m sf) =
        Just (maybe (pr' <+> sf') ((pr' <+> sf') <+>) m')
      where
        pr' = digit (<+>) f s pr
        sf' = digit (<+>) f sPsprm sf
        m' = foldToMaybeWithIndexTree' (<+>) (node (<+>) f) sPspr m
        !sPspr = s + size pr
        !sPsprm = sPspr + size m
    {-# SPECIALISE digit :: (b -> b -> b) -> (Int -> Elem y -> b) -> Int -> Digit (Elem y) -> b #-}
    {-# SPECIALISE digit :: (b -> b -> b) -> (Int -> Node y -> b) -> Int -> Digit (Node y) -> b #-}
    digit :: Sized a => (b -> b -> b) -> (Int -> a -> b) -> Int -> Digit a -> b
    digit = foldWithIndexDigit
    {-# SPECIALISE node :: (b -> b -> b) -> (Int -> Elem y -> b) -> Int -> Node (Elem y) -> b #-}
    {-# SPECIALISE node :: (b -> b -> b) -> (Int -> Node y -> b) -> Int -> Node (Node y) -> b #-}
    node :: Sized a => (b -> b -> b) -> (Int -> a -> b) -> Int -> Node a -> b
    node = foldWithIndexNode

-- | 'toPQS', given an ordering function, converts a 'Seq' to a
-- 'PQS'.
toPQS :: (b -> b -> Ordering) -> (Int -> Elem y -> PQS b) -> Int -> FingerTree (Elem y) -> Maybe (PQS b)
toPQS cmp = foldToMaybeWithIndexTree (mergePQS cmp)

-- | 'mergePQS' merges two PQS, taking into account the original
-- position of the elements.
mergePQS :: (a -> a -> Ordering) -> PQS a -> PQS a -> PQS a
mergePQS cmp q1@(PQS i1 x1 ts1) q2@(PQS i2 x2 ts2) =
    case cmp x1 x2 of
        LT -> PQS i1 x1 (q2 :&& ts1)
        EQ | i1 <= i2 -> PQS i1 x1 (q2 :&& ts1)
        _ -> PQS i2 x2 (q1 :&& ts2)

data PQT a b = PQT a b (PQTL a b)
data PQTL a b = NilT | {-# UNPACK #-} !(PQT a b) :&&& PQTL a b

infixr 8 :&&&

popMinT :: (a -> a -> Ordering) -> State (PQT a b) b
popMinT cmp = State unrollPQ'
  where
    {-# INLINE unrollPQ' #-}
    unrollPQ' (PQT _ x ts) = (mergePQs ts, x)
    mergePQs (t :&&& NilT) = t
    mergePQs (t1 :&&& t2 :&&& NilT) = t1 <+> t2
    mergePQs (t1 :&&& t2 :&&& ts) = (t1 <+> t2) <+> mergePQs ts
    mergePQs NilT = error "popMin: tried to pop from empty queue"
    (<+>) = mergePQT cmp

-- | 'mergePQT' merges two 'PQT's.
mergePQT :: (a -> a -> Ordering) -> PQT a b -> PQT a b -> PQT a b
mergePQT cmp q1@(PQT x1 y1 ts1) q2@(PQT x2 y2 ts2)
  | cmp x1 x2 == GT     = PQT x2 y2 (q1 :&&& ts2)
  | otherwise           = PQT x1 y1 (q2 :&&& ts1)

toPQT :: (b -> b -> Ordering) -> (a -> PQT b c) -> FingerTree a -> Maybe (PQT b c)
toPQT cmp = foldToMaybeTree (mergePQT cmp)

data PQST e a = PQST {-# UNPACK #-} !Int e a (PQSTL e a)
data PQSTL e a = NlT | {-# UNPACK #-} !(PQST e a) :&&&& PQSTL e a

infixr 8 :&&&&

-- | 'popMinS', given an ordering function, constructs a stateful action
-- which pops the smallest elements from a queue. This action will fail
-- on empty queues.
popMinST :: (e -> e -> Ordering) -> State (PQST e a) a
popMinST cmp = State unrollPQ'
  where
    {-# INLINE unrollPQ' #-}
    unrollPQ' (PQST _ _ x ts) = (mergePQs ts, x)
    mergePQs (t :&&&& NlT) = t
    mergePQs (t1 :&&&& t2 :&&&& NlT) = t1 <+> t2
    mergePQs (t1 :&&&& t2 :&&&& ts) = (t1 <+> t2) <+> mergePQs ts
    mergePQs NlT = error "popMin: tried to pop from empty queue"
    (<+>) = mergePQST cmp

-- | 'toPQS', given an ordering function, converts a 'Seq' to a
-- 'PQS'.
toPQST :: (b -> b -> Ordering) -> (Int -> Elem y -> PQST b c) -> Int -> FingerTree (Elem y) -> Maybe (PQST b c)
toPQST cmp = foldToMaybeWithIndexTree (mergePQST cmp)

-- | 'mergePQS' merges two PQS, taking into account the original
-- position of the elements.
mergePQST :: (a -> a -> Ordering) -> PQST a b -> PQST a b -> PQST a b
mergePQST cmp q1@(PQST i1 x1 y1 ts1) q2@(PQST i2 x2 y2 ts2) =
    case cmp x1 x2 of
        LT -> PQST i1 x1 y1 (q2 :&&&& ts1)
        EQ | i1 <= i2 -> PQST i1 x1 y1 (q2 :&&&& ts1)
        _ -> PQST i2 x2 y2 (q1 :&&&& ts2)
