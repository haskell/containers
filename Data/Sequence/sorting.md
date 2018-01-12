# Sorting

Data.Sequence exports two methods of sorting: stable and unstable. The stable sort is simply a call to Data.List.Sort, whereas the unstable sort constructs a pairing heap, and uses it to perform heap sort.

The pairing heap seems to particularly suit the structure of the finger tree, as other heaps have not managed to beat it. Specifically, when compared to a skew heap:

```haskell
unstableSortBy :: (a -> a -> Ordering) -> Seq a -> Seq a
unstableSortBy cmp (Seq xs) =
    execState (replicateA (size xs) (popMin cmp)) (toSkew cmp (Seq xs))

data Skew a = Nil | Br a !(Skew a) !(Skew a)

popMin :: (e -> e -> Ordering) -> State (Skew e) e
popMin cmp = State unrollPQ'
  where
    {-# INLINE unrollPQ' #-}
    unrollPQ' (Br x ls rs) = (mergeSkew cmp ls rs, x)

toSkew :: (e -> e -> Ordering) -> Seq e -> Skew e
toSkew cmp (Seq xs') = toSkewTree cmp (\(Elem a) -> Br a Nil Nil) xs'
  where
    toSkewTree :: (b -> b -> Ordering) -> (a -> Skew b) -> FingerTree a -> Skew b
    toSkewTree _ _ EmptyT = Nil
    toSkewTree _ f (Single xs) = f xs
    toSkewTree cmp f (Deep n pr m sf) = pr' <+> sf' <+> m'
      where
        pr' = toSkewDigit cmp f pr
        sf' = toSkewDigit cmp f sf
        m' = toSkewTree cmp (toSkewNode cmp f) m
        (<+>) = mergeSkew cmp
    toSkewDigit :: (b -> b -> Ordering) -> (a -> Skew b) -> Digit a -> Skew b
    toSkewDigit cmp f dig =
        case dig of
            One a -> f a
            Two a b -> f a <+> f b
            Three a b c -> f a <+> f b <+> f c
            Four a b c d -> (f a <+> f b) <+> (f c <+> f d)
      where
        (<+>) = mergeSkew cmp
    toSkewNode cmp f node =
        case node of
            Node2 _ a b -> f a <+> f b
            Node3 _ a b c -> f a <+> f b <+> f c
      where
        (<+>) = mergeSkew cmp

mergeSkew :: (a -> a -> Ordering) -> Skew a -> Skew a -> Skew a
mergeSkew cmp Nil ys = ys
mergeSkew cmp xs Nil = xs
mergeSkew cmp h1@(Br x lx rx) h2@(Br y ly ry)
  | cmp x y == GT = Br y (mergeSkew cmp h1 ry) ly
  | otherwise     = Br x (mergeSkew cmp h2 rx) lx
```

The pairing heap implementation is faster in every aspect:

```
benchmarking 1000000/unsorted/pairing
time                 2.005 s    (NaN s .. 2.102 s)
                     1.000 R²   (0.998 R² .. 1.000 R²)
mean                 2.069 s    (2.060 s .. 2.075 s)
std dev              9.340 ms   (0.0 s .. 10.67 ms)
variance introduced by outliers: 19% (moderately inflated)
             
benchmarking 1000000/unsorted/skew
time                 2.042 s    (1.637 s .. 2.267 s)
                     0.995 R²   (0.990 R² .. NaN R²)
mean                 2.165 s    (2.065 s .. 2.217 s)
std dev              87.10 ms   (0.0 s .. 91.26 ms)
variance introduced by outliers: 19% (moderately inflated)
             
benchmarking 1000000/ascending/pairing
time                 191.4 ms   (187.8 ms .. 193.5 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 197.0 ms   (194.7 ms .. 200.0 ms)
std dev              3.221 ms   (2.441 ms .. 3.924 ms)
variance introduced by outliers: 14% (moderately inflated)
             
benchmarking 1000000/ascending/skew
time                 232.3 ms   (227.0 ms .. 238.9 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 233.9 ms   (230.6 ms .. 236.2 ms)
std dev              3.678 ms   (2.790 ms .. 4.777 ms)
variance introduced by outliers: 14% (moderately inflated)
             
benchmarking 1000000/descending/pairing
time                 204.6 ms   (190.2 ms .. 214.1 ms)
                     0.998 R²   (0.991 R² .. 1.000 R²)
mean                 208.4 ms   (204.1 ms .. 210.6 ms)
std dev              4.051 ms   (1.299 ms .. 5.288 ms)
variance introduced by outliers: 14% (moderately inflated)
             
benchmarking 1000000/descending/skew
time                 229.9 ms   (212.7 ms .. 240.1 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 238.8 ms   (231.3 ms .. 241.4 ms)
std dev              5.006 ms   (269.0 μs .. 6.151 ms)
variance introduced by outliers: 16% (moderately inflated)
```
