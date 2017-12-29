Sequences
=========

.. highlight:: haskell

Sequences allow you to store a finite number of sequential elements, they are
similar to deques (double ended queues) or vectors in other languages. They
support efficient prepending, appending, and range construction. The relevant
module from the ``containers`` package is `Data.Sequence
<http://hackage.haskell.org/package/containers/docs/Data-Sequence.html>`_.


Example
-------

The following GHCi session shows some of the basic sequence funcitonality::

    -- Import the Seq type and operators for combining sequences unqualified.
    -- Import the rest of the Sequence module qualified.
    import Data.Sequence (Seq(..), (<|), (|>), (><))
    import qualified Data.Sequence as Seq

    let nums = Seq.fromList [1, 2, 3]


    -- Put numbers on the front and back.
    0 <| nums
    > fromList [0,1,2,3]

    nums |> 4
    > fromList [1,2,3,4]


    -- Put two sequences together.
    (Seq.fromList [-2, -1]) >< (Seq.fromList [0, 1, 2])
    > fromList [-2,-1,0,1,2]


    -- Check if a sequence is empty and check the length.
    Seq.null nums
    > False

    Seq.length nums
    > 3


    -- Lookup an element at a certain index (since version 0.4.8).
    Seq.lookup 2 nums
    > Just 3

    -- Or the unsafe version, you MUST check length beforehand.
    Seq.index 2 nums
    > 3


Importing Sequence
------------------

When using ``Sequence`` in a Haskell source file you should always use a
``qualified`` import becasue it exports names that clash with the standard
Prelude (you can import the type constructor and some operators on their own
though!).

::

    import Data.Sequence (Seq(..), (<|), (|>), (><))
    import qualified Data.Sequence as Seq


Common API Functions
--------------------

.. NOTE::
   ``fromList [some,sequence,elements]`` is how a ``Seq`` is printed.

Construction and Conversion
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an empty sequence
""""""""""""""""""""""""

::

    Seq.empty :: Seq a
    Seq.empty = ...

Creates a sequence with zero elements.

::

    Seq.empty
    > fromList []


Create a sequence with one element (singleton)
""""""""""""""""""""""""""""""""""""""""""""""

::

    Seq.singleton :: a -> Seq a
    Seq.singleton x = ...

Creates a sequence with the single element ``x`` in it.

::

    Seq.singleton "containers"
    > fromList ["containers"]

    Seq.singleton 1
    > fromList [1]

Create a sequence with the same element repeated
""""""""""""""""""""""""""""""""""""""""""""""""

::

    Seq.replicate :: Int -> a -> Seq a
    Seq.replicate n x = ...

Creates a sequence with same element ``x`` repeated ``n`` times.

::

    Seq.replicate 0 "hi"
    > fromList []

    Seq.replicate 3 "hi"
    > fromList ["hi","hi","hi"]

Create a sequence from a list
"""""""""""""""""""""""""""""

::

    Seq.fromList :: [a] -> Seq a
    Seq.FromList xs = ...

Creates a sequence containing the elements of the list ``xs``. Sequences allow
duplicate so all elements will be included in the order given.

::

    Seq.fromList ["base", "containers", "QuickCheck"]
    > fromList ["base","containers","QuickCheck"]

    Seq.fromList [0, 1, 1, 2, 3, 1]
    > fromList [0,1,1,2,3,1]

Adding to an existing sequence
""""""""""""""""""""""""""""""

::

    (<|) :: a -> Seq a -> Seq a
    x <| xs = ...

    (|>) :: Seq a -> a -> Seq a
    xs |> x = ...

    (><) :: Seq a -> Seq a -> Seq a
    l >< r = ...

- ``x <| xs`` places the element ``x`` at the beginning of the sequence ``xs``.

- ``xs |> x`` places the element ``x`` at the end of the sequence ``xs``

- ``l >< r`` combines the two sequences ``l`` and ``r`` together.


Create a list from a sequence
"""""""""""""""""""""""""""""

::

    foldr (:) [] :: Seq a -> [a]


There is no ``toList`` function in the Sequence module since it can be
`easily implemented <https://wiki.haskell.org/Foldable_and_Traversable>`_ with a
fold using ``Seq``'s `Foldable
<https://wiki.haskell.org/Typeclassopedia#Foldable>`_ instance.

::

    foldr (:) [] (Seq.fromList ["base", "containers", "QuickCheck"])
    > ["base","containers","QuickCheck"]


Querying
^^^^^^^^

Check if a sequence is empty
""""""""""""""""""""""""""""

::

    Seq.null :: Seq a -> Bool
    Seq.null xs = ...

   Returns ``True`` if the sequence ``xs`` is empty, ``False`` otherwise.

::

    Seq.null Seq.empty
    > True

    Seq.null (Seq.fromList [1, 2, 3])
    > False

The length/size of a sequence
"""""""""""""""""""""""""""""

::

    Seq.length :: Seq a -> Int
    Seq.length xs = ...

Returns the length of the sequence ``xs``.

::

    Seq.length Seq.empty
    > 0

    Seq.size (Seq.fromList [1, 2, 3])
    > 3

The element at a given index
""""""""""""""""""""""""""""

::

    Seq.lookup :: Int -> Seq a -> Maybe a
    Seq.lookup n xs = ...

    Seq.!? :: Seq a -> Int -> Maybe a
    xs `Seq.!?` n = ...

Returns the element at the position ``n``, ``Nothing`` if the index is out of
bounds. ``!?`` is simply a flipped version of ``lookup``.

``Seq.index :: Seq a -> Int -> a``

Returns the element at the given position, throws a runtime ``error`` if the
index is out of bounds.

.. NOTE::
   Use ``lookup``/``!?`` whenever you can and explicitly deal with the
   ``Nothing`` case.

::

    (Seq.fromList ["base", "containers"]) Seq.!? 0
    > Just "base"

    Seq.index 0 (Seq.fromList ["base", "containers"])
    > "base"

    (Seq.fromList ["base", "containers"]) Seq.!? 2
    > Nothing

    Seq.index (Seq.fromList ["base", "containers"]) 2
    > "*** Exception: index out of bounds


Modification
^^^^^^^^^^^^

Inserting an element
""""""""""""""""""""

::

    Seq.insertAt :: Int -> a -> Seq a -> Seq a
    Seq.insertAt i x xs = ...

Inserts ``x`` into ``xs`` at the index ``i``, shifting the rest of the sequence
over.

::

    Seq.insertAt 0 "idris" (Seq.fromList ["haskell", "rust"])
    > fromList ["idris","haskell","rust"]

See also `Adding to an existing sequence`_.

Replace an element
""""""""""""""""""

::

    Seq.update :: Int -> a -> Seq a -> Seq a
    Seq.update i x xs = ...

Replaces the element at position ``i`` in the sequence with ``x``. If the index
is out of bounds then the original sequence is returned.

::

    Seq.update 0 "hello" (Seq.fromList ["hi", "world", "!"])
    > fromList ["hello","world","!"]

    Seq.update 3 "OUTOFBOUNDS" (Seq.fromList ["hi", "world", "!"])
    > fromList ["hi","world","!"]

Adjust/modify an element
""""""""""""""""""""""""

*Since version 0.5.8*

::

    adjust' :: forall a. (a -> a) -> Int -> Seq a -> Seq a
    adjust f i xs = ...

Updates the element at position ``i`` in the sequence by applying the function
``f`` to the existing element. If the index is out of bounds then the original
sequence is returned.

::

    Seq.adjust' (*10) 0 (Seq.fromList [1, 2, 3])
    > fromList [10,2,3]

    Seq.adjust (*10) 3 (Seq.fromList [1, 2, 3])
    > fromList [1,2,3]

.. NOTE::
   If you're using an older version of containers which only has ``adjust``, be
   careful because it can lead to `poor performance and space leaks
   <http://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-Sequence.html#v:adjust>`_.

Delete an element
"""""""""""""""""

::

    deleteAt :: Int -> Seq a -> Seq a
    deleteAt i xs = ...

Deletes the element of the sequence at index ``i``.


Sorting
^^^^^^^

::

    sort :: Ord a => Seq a -> Seq a
    sort xs = ...

Sorts the sequence ``xs`` using the ``Ord`` instance.

::

    Seq.sort (Seq.fromList ["x", "a", "c", "b"])
    > fromList ["a","b","c","x"]


Subsequences
^^^^^^^^^^^^

Take
""""

::

    Seq.take :: Int -> Seq a -> Seq a
    Seq.take n xs = ...

Returns the first ``n`` elements of the sequence ``xs``. If the length of ``xs``
is less than ``n`` then all elements are returned.

::

    Seq.take 0 (Seq.fromList [1, 2, 3])
    > fromList []

    Seq.take 2 (Seq.fromList [1, 2, 3])
    > fromList [1,2]

    Seq.take 5 (Seq.fromList [1, 2, 3])
    > fromList [1,2,3]

Drop
""""

::

    Seq.drop :: Int -> Seq a -> Seq a
    Seq.drop n xs = ...

Drops the first ``n`` elements of the sequence ``xs``. If the length of ``xs``
is less than ``n`` then an empty sequence is returned.

::

    Seq.drop 0 (Seq.fromList [1, 2, 3])
    > fromList [1,2,3]

    Seq.drop 2 (Seq.fromList [1, 2, 3])
    > fromList [3]

    Seq.drop 5 (Seq.fromList [1, 2, 3])
    > fromList []

Chunks
""""""

::

    Seq.chunksOf :: Int -> Seq a -> Seq (Seq a)
    Seq.chunksOf k xs = ...

Splits the sequence ``xs`` into chunks of size ``k``. If the length of the
sequence is not divisible by ``k`` then the last chunk will have less than ``k``
elements.

.. WARNING::
   ``k`` can only be ``0`` when the sequence is empty, otherwise a runtime
   ``error`` is thrown.

::

    -- A chunk size of 0 can ONLY be given for an empty sequence.
    Seq.chunksOf 0 Seq.empty
    > fromList []

    Seq.chunksOf 1 (Seq.fromList [1, 2, 3])
    > fromList [fromList [1],fromList [2],fromList [3]]

    Seq.chunksOf 2 (Seq.fromList [1, 2, 3])
    > fromList [fromList [1,2],fromList [3]]

    Seq.chunksOf 5 (Seq.fromList [1, 2, 3])
    > fromList [fromList [1,2,3]]


Typeclass Instances
-------------------

``Seq`` is an instance of a number of common typeclasses, for the full list see
the `docs
<http://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-Sequence.html#t:Seq>`_.

.. NOTE::
   Some constraints have been left out for brevity, and the types given below
   are speciliazed to ``Seq``; the true types are more general.

- `Show
  <http://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#t:Show>`_ -
  conversion to string: ``show :: Seq a -> String``
- `Eq
  <http://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#t:Eq>`_ -
  equality check: ``(==) :: Seq a -> Seq a -> Bool``
- `Ord
  <http://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#t:Ord>`_ -
  comparison: ``(<) :: Seq a -> Seq a -> Bool``
- `Foldable <https://wiki.haskell.org/Typeclassopedia#Foldable>`_ - collapse
  into summary value: ``foldr :: (a -> b -> b) -> b -> Seq a -> b``
- `Semigroup <https://wiki.haskell.org/Typeclassopedia#Semigroup>`_ - combine
  two things together: ``(<>) :: Seq a -> Seq a -> Seq a``
- `Monoid <https://wiki.haskell.org/Typeclassopedia#Monoid>`_  - a semigroup
  with an identity element: ``mempty :: Seq a``
- `Functor <https://wiki.haskell.org/Typeclassopedia#Functor>`_ - a container
  that can be mapped over: ``fmap :: (a -> b) -> Seq a -> Seq b``
- `Traversable <https://wiki.haskell.org/Typeclassopedia#Traversable>`_ - a
  functor with effects, follow the link :)
- `Applicative <https://wiki.haskell.org/Typeclassopedia#Applicative>`_ - follow
  the link :)
- `Monad <https://wiki.haskell.org/Typeclassopedia#Monad>`_ - follow
  the link :)


Serialization
-------------

The best way to serialize and deserialize sequences is to use one of the many
libraries which already supports serializing sequences. `binary
<https://hackage.haskell.org/package/binary>`_, `cereal
<https://hackage.haskell.org/package/cereal>`_, and `store
<https://hackage.haskell.org/package/store>`_ are some common libraries that
people use.


Performance
-----------

The API docs are annotated with the Big-*O* complexities of each of the sequence
operations. For benchmarks see the `haskell-perf/sequences
<https://github.com/haskell-perf/sequences>`_ page.


Looking for more?
-----------------

Didn't find what you're looking for? This tutorial only covered the most common
sequence functions, for a full list of functions see the `Sequence
<http://hackage.haskell.org/package/containers/docs/Data-Sequence.html>`_ API
documentation.
