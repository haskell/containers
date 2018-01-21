{-# LANGUAGE BangPatterns #-}

-- |
--
-- Further notes are available in the file sorting.md (in this
-- directory).

module Data.Sequence.Internal.Sorting
  (
   -- * Sort Functions
   sort
  ,sortBy
  ,sortOn
  ,unstableSort
  ,unstableSortBy
  ,unstableSortOn
  ,
   -- * Heaps
   Queue(..)
  ,QList(..)
  ,IndexedQueue(..)
  ,IQList(..)
  ,TaggedQueue(..)
  ,TQList(..)
  ,IndexedTaggedQueue(..)
  ,ITQList(..)
  ,
   -- * Merging
   mergeQ
  ,mergeIQ
  ,mergeTQ
  ,mergeITQ
  ,
   -- * popMin
   popMinQ
  ,popMinIQ
  ,popMinTQ
  ,popMinITQ
  ,
   -- * Building
   buildQ
  ,buildIQ
  ,buildTQ
  ,buildITQ
  ,
   -- * Special folds
   foldToMaybeTree
  ,foldToMaybeWithIndexTree)
  where

import Data.Sequence.Internal
       (Elem(..), Seq(..), Node(..), Digit(..), Sized(..), FingerTree(..),
        State(..), execState, replicateA, foldDigit, foldNode,
        foldWithIndexDigit, foldWithIndexNode)

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
        (execState (replicateA (size xs) (popMinIQ cmp)))
        (buildIQ cmp (\s (Elem x) -> IQ s x IQNil) 0 xs)

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
       (execState (replicateA (size xs) (popMinITQ compare)))
       (buildITQ compare (\s (Elem x) -> ITQ s (f x) x ITQNil) 0 xs)

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
        (execState (replicateA (size xs) (popMinQ cmp)))
        (buildQ cmp (\(Elem x) -> Q x Nil) xs)

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
       (execState (replicateA (size xs) (popMinTQ compare)))
       (buildTQ compare (\(Elem x) -> TQ (f x) x TQNil) xs)

------------------------------------------------------------------------
-- Heaps
--
-- The following are definitions for various specialized pairing heaps.
------------------------------------------------------------------------

-- | A 'Queue' is a simple pairing heap.
data Queue e = Q !e (QList e)
data QList e
    = Nil
    | QCons {-# UNPACK #-} !(Queue e)
            (QList e)

-- | A pairing heap tagged with the original position of elements,
-- to allow for stable sorting.
data IndexedQueue e =
    IQ {-# UNPACK #-} !Int !e (IQList e)
data IQList e
    = IQNil
    | IQCons {-# UNPACK #-} !(IndexedQueue e)
             (IQList e)

-- | A pairing heap tagged with some key for sorting elements, for use
-- in 'unstableSortOn'.
data TaggedQueue a b =
    TQ !a b (TQList a b)
data TQList a b
    = TQNil
    | TQCons {-# UNPACK #-} !(TaggedQueue a b)
             (TQList a b)

-- | A pairing heap tagged with both a key and the original position
-- of its elements, for use in 'sortOn'.
data IndexedTaggedQueue e a =
    ITQ {-# UNPACK #-} !Int !e a (ITQList e a)
data ITQList e a
    = ITQNil
    | ITQCons {-# UNPACK #-} !(IndexedTaggedQueue e a)
              (ITQList e a)

infixr 8 `ITQCons`, `TQCons`, `QCons`, `IQCons`

------------------------------------------------------------------------
-- Merges
--
-- The following are definitions for "merge" for each of the heaps
-- above.
------------------------------------------------------------------------

-- | 'mergeQ' merges two 'Queue's.
mergeQ :: (a -> a -> Ordering) -> Queue a -> Queue a -> Queue a
mergeQ cmp q1@(Q x1 ts1) q2@(Q x2 ts2)
  | cmp x1 x2 == GT     = Q x2 (q1 `QCons` ts2)
  | otherwise           = Q x1 (q2 `QCons` ts1)

-- | 'mergeTQ' merges two 'TaggedQueue's.
mergeTQ :: (a -> a -> Ordering)
        -> TaggedQueue a b
        -> TaggedQueue a b
        -> TaggedQueue a b
mergeTQ cmp q1@(TQ x1 y1 ts1) q2@(TQ x2 y2 ts2)
  | cmp x1 x2 == GT     = TQ x2 y2 (q1 `TQCons` ts2)
  | otherwise           = TQ x1 y1 (q2 `TQCons` ts1)

-- | 'mergeIQ' merges two IndexedQueue, taking into account the original
-- position of the elements.
mergeIQ :: (a -> a -> Ordering)
        -> IndexedQueue a
        -> IndexedQueue a
        -> IndexedQueue a
mergeIQ cmp q1@(IQ i1 x1 ts1) q2@(IQ i2 x2 ts2) =
    case cmp x1 x2 of
        LT -> IQ i1 x1 (q2 `IQCons` ts1)
        EQ | i1 <= i2 -> IQ i1 x1 (q2 `IQCons` ts1)
        _ -> IQ i2 x2 (q1 `IQCons` ts2)

-- | 'mergeIQ' merges two IndexedQueue, taking into account the original
-- position of the elements.
mergeITQ
    :: (a -> a -> Ordering)
    -> IndexedTaggedQueue a b
    -> IndexedTaggedQueue a b
    -> IndexedTaggedQueue a b
mergeITQ cmp q1@(ITQ i1 x1 y1 ts1) q2@(ITQ i2 x2 y2 ts2) =
    case cmp x1 x2 of
        LT -> ITQ i1 x1 y1 (q2 `ITQCons` ts1)
        EQ | i1 <= i2 -> ITQ i1 x1 y1 (q2 `ITQCons` ts1)
        _ -> ITQ i2 x2 y2 (q1 `ITQCons` ts2)

------------------------------------------------------------------------
-- popMin
--
-- The following are definitions for @popMin@, a function which
-- constructs a stateful action which pops the next element from a
-- queue. This action will fail on empty queues.
------------------------------------------------------------------------

popMinQ :: (e -> e -> Ordering) -> State (Queue e) e
popMinQ cmp = State unrollPQ'
  where
    {-# INLINE unrollPQ' #-}
    unrollPQ' (Q x ts) = (mergeQs ts, x)
    mergeQs (t `QCons` Nil) = t
    mergeQs (t1 `QCons` t2 `QCons` Nil) = t1 <+> t2
    mergeQs (t1 `QCons` t2 `QCons` ts) = (t1 <+> t2) <+> mergeQs ts
    mergeQs Nil = error "popMinQ: tried to pop from empty queue"
    (<+>) = mergeQ cmp

popMinIQ :: (e -> e -> Ordering) -> State (IndexedQueue e) e
popMinIQ cmp = State unrollPQ'
  where
    {-# INLINE unrollPQ' #-}
    unrollPQ' (IQ _ x ts) = (mergeQs ts, x)
    mergeQs (t `IQCons` IQNil) = t
    mergeQs (t1 `IQCons` t2 `IQCons` IQNil) = t1 <+> t2
    mergeQs (t1 `IQCons` t2 `IQCons` ts) = (t1 <+> t2) <+> mergeQs ts
    mergeQs IQNil = error "popMinQ: tried to pop from empty queue"
    (<+>) = mergeIQ cmp

popMinTQ :: (a -> a -> Ordering) -> State (TaggedQueue a b) b
popMinTQ cmp = State unrollPQ'
  where
    {-# INLINE unrollPQ' #-}
    unrollPQ' (TQ _ x ts) = (mergeQs ts, x)
    mergeQs (t `TQCons` TQNil) = t
    mergeQs (t1 `TQCons` t2 `TQCons` TQNil) = t1 <+> t2
    mergeQs (t1 `TQCons` t2 `TQCons` ts) = (t1 <+> t2) <+> mergeQs ts
    mergeQs TQNil = error "popMinQ: tried to pop from empty queue"
    (<+>) = mergeTQ cmp

popMinITQ :: (e -> e -> Ordering) -> State (IndexedTaggedQueue e a) a
popMinITQ cmp = State unrollPQ'
  where
    {-# INLINE unrollPQ' #-}
    unrollPQ' (ITQ _ _ x ts) = (mergeQs ts, x)
    mergeQs (t `ITQCons` ITQNil) = t
    mergeQs (t1 `ITQCons` t2 `ITQCons` ITQNil) = t1 <+> t2
    mergeQs (t1 `ITQCons` t2 `ITQCons` ts) = (t1 <+> t2) <+> mergeQs ts
    mergeQs ITQNil = error "popMinQ: tried to pop from empty queue"
    (<+>) = mergeITQ cmp

------------------------------------------------------------------------
-- to*
--
-- The following are definitions for @to*@, a function which
-- constructs queue from a 'Seq'.
------------------------------------------------------------------------

buildQ :: (b -> b -> Ordering) -> (a -> Queue b) -> FingerTree a -> Maybe (Queue b)
buildQ cmp = foldToMaybeTree (mergeQ cmp)

buildIQ
    :: (b -> b -> Ordering)
    -> (Int -> Elem y -> IndexedQueue b)
    -> Int
    -> FingerTree (Elem y)
    -> Maybe (IndexedQueue b)
buildIQ cmp = foldToMaybeWithIndexTree (mergeIQ cmp)

buildTQ
    :: (b -> b -> Ordering)
    -> (a -> TaggedQueue b c)
    -> FingerTree a
    -> Maybe (TaggedQueue b c)
buildTQ cmp = foldToMaybeTree (mergeTQ cmp)

buildITQ
    :: (b -> b -> Ordering)
    -> (Int -> Elem y -> IndexedTaggedQueue b c)
    -> Int
    -> FingerTree (Elem y)
    -> Maybe (IndexedTaggedQueue b c)
buildITQ cmp = foldToMaybeWithIndexTree (mergeITQ cmp)

------------------------------------------------------------------------
-- foldToMaybe
--
-- Definitions for foldToMaybe, which are used in constructing the
-- queues above.
------------------------------------------------------------------------

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

{-# INLINE foldToMaybeWithIndexTree #-}
foldToMaybeWithIndexTree :: (b -> b -> b)
                         -> (Int -> Elem y -> b)
                         -> Int
                         -> FingerTree (Elem y)
                         -> Maybe b
foldToMaybeWithIndexTree = foldToMaybeWithIndexTree'
  where
    {-# SPECIALISE foldToMaybeWithIndexTree' :: (b -> b -> b) -> (Int -> Elem y -> b) -> Int -> FingerTree (Elem y) -> Maybe b #-}
    {-# SPECIALISE foldToMaybeWithIndexTree' :: (b -> b -> b) -> (Int -> Node y -> b) -> Int -> FingerTree (Node y) -> Maybe b #-}
    foldToMaybeWithIndexTree'
        :: Sized a
        => (b -> b -> b) -> (Int -> a -> b) -> Int -> FingerTree a -> Maybe b
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
    digit
        :: Sized a
        => (b -> b -> b) -> (Int -> a -> b) -> Int -> Digit a -> b
    digit = foldWithIndexDigit
    {-# SPECIALISE node :: (b -> b -> b) -> (Int -> Elem y -> b) -> Int -> Node (Elem y) -> b #-}
    {-# SPECIALISE node :: (b -> b -> b) -> (Int -> Node y -> b) -> Int -> Node (Node y) -> b #-}
    node
        :: Sized a
        => (b -> b -> b) -> (Int -> a -> b) -> Int -> Node a -> b
    node = foldWithIndexNode
