Sets
====

.. highlight:: haskell

Sets allow you to store non-repeated ordered elements, providing efficient
insertion, lookups, deletions, and set operations. There are two implementations
provided by the ``containers`` package: `Data.Set
<http://hackage.haskell.org/package/containers/docs/Data-Set.html>`_ and
`Data.IntSet
<http://hackage.haskell.org/package/containers/docs/Data-IntSet.html>`_. Use
``IntSet`` if you are storing, well... ``Int`` s.


Example
-------

The following GHCi session shows some of the basic set functionality::

    import qualified Data.Set as Set

    let dataStructures = Set.fromList ["Set", "Map", "Graph", "Sequence"]

    -- Check if "Map" and "Trie" are in the set of data structures.
    Set.member "Map" dataStructures
    > True

    Set.member "Trie" dataStructures
    > False


    -- Add "Trie" to our original set of data structures.
    let moreDataStructures = Set.insert "Trie" dataStructures

    Set.member "Trie" moreDataStructures
    > True


    -- Remove "Graph" froum our original set of data structures.
    let fewerDataStructures = Set.delete "Graph" dataStructures

    Set.toAscList fewerDataStructures
    > ["Map","Sequence","Set"]


    -- Create a new set and combine it with our original set.
    let unorderedDataStructures = Set.fromList ["HashSet", "HashMap"]

    Set.union dataStructures unorderedDataStructures
    > fromList ["Graph","HashMap","HashSet","Map","Sequence","Set"]


Importing Set and IntSet
------------------------

When using ``Set`` or ``IntSet`` in a Haskell source file you should always use
a ``qualified`` import because they export names that clash with the standard
Prelude (you can import the type constructor on its own though!):

::

    import Data.Set (Set)
    import qualified Data.Set as Set

    import Data.IntSet (IntSet)
    import qualified Data.IntSet as IntSet


Common API Functions
--------------------

.. TIP::
   All of these functions that work for ``Set`` will also work for ``IntSet``,
   which has the element type ``a`` specialized to ``Int``. Anywhere that you
   see ``Set a`` you can replace it with ``IntSet``.

.. NOTE::
   ``fromList [some,list,elements]`` is how a ``Set`` is printed.


Construction and Conversion
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an empty set
"""""""""""""""""""

::

    Set.empty :: Set a
    Set.empty = ...

Creates a set with zero elements.

::

    Set.empty
    > fromList []

Create a set with one element (singleton)
"""""""""""""""""""""""""""""""""""""""""

::

    Set.singleton :: a -> Set a
    Set.singleton x = ...

Creates a set with a single element ``x`` in it.

::

    Set.singleton "containers"
    > fromList ["containers"]

    Set.singleton 1
    > fromList [1]

Create a set from a list
""""""""""""""""""""""""

::

    Set.fromList :: Ord a => [a] -> Set a
    Set.fromList xs = ...

Creates a set containing the elements of the list ``xs``. Since sets don't
contain duplicates, if there are repeated elements in the list they will only
appear once.

::

    Set.fromList ["base", "containers", "QuickCheck"]
    > fromList ["QuickCheck","base","containers"]

    Set.fromList [1, 1, 2, 3, 4, 4, 5, 1]
    > fromList [1,2,3,4,5]

Create a list from a set
""""""""""""""""""""""""

::

    Set.toAscList, Set.toList, Set.elems :: Set a -> [a]
    Set.toAscList s = ...

Returns a list containing the elements of the set ``s`` in *ascending* order.

.. NOTE::
   These all do the same thing, use ``toAscList`` because its name indicates the
   ordering.

::

    Set.toDescList :: Set a -> [a]
    Set.toDescList s = ...

Returns a list containing the elements of the set ``s`` in *descending* order.

::

    Set.toAscList (Set.fromList [0, 2, 4, 6])
    > [0,2,4,6]

    Set.toDescList (Set.fromList [0, 2, 4, 6]
    > [6,4,2,0]


Querying
^^^^^^^^

Check if a set is empty
"""""""""""""""""""""""

::

    Set.null :: Set a -> Bool
    Set.null s = ...

Returns ``True`` if the set ``s`` is empty, ``False`` otherwise.

::

    Set.null Set.empty
    > True

    Set.null (Set.fromList [0, 2, 4, 6])
    > False


The number of elements in a set
"""""""""""""""""""""""""""""""

::

    Set.size :: Set a -> Int
    Set.size s = ...

Returns the number of elements in the set ``s``.

::

    Set.size Set.empty
    > 0

    Set.size (Set.fromList [0, 2, 4, 6])
    > 4

Check if an element is in a set (member)
""""""""""""""""""""""""""""""""""""""""

::

    Set.member :: Ord a => a -> Set a -> Bool
    Set.member x s = ...

Returns ``True`` if the element ``x`` is in the set ``s``, ``False`` otherwise.

::

    Set.member 0 Set.empty
    > False

    Set.member 0 (Set.fromList [0, 2, 4, 6])
    > True

Find the minimum/maximum element in a set
"""""""""""""""""""""""""""""""""""""""""

*Since version 0.5.9*

::

   lookupMin, lookupMax :: Set a -> Maybe a
   lookupMin s = ...
   lookupMax s = ...

Returns the minimum, or maximum respectively, element of the set ``s``, or
``Nothing`` if the set is empty.

::

    Set.lookupMin Set.empty
    > Nothing

    Set.lookupMin (Set.fromList [0, 2, 4, 6])
    > Just 0

    Set.lookupMax (Set.fromList [0, 2, 4, 6])
    > Just 6

.. WARNING::
   Unless you're using an old version of ``containers`` **DO NOT** use
   ``Set.findMin`` or ``Set.findMax``, they are partial and throw a runtime
   error if the set is empty.

Modification
^^^^^^^^^^^^

Adding a new element to a set
"""""""""""""""""""""""""""""

::

    Set.insert :: Ord a => a -> Set a -> Set a
    Set.insert x s = ...

Inserts the element ``x`` into the set ``s``, replacing an existing equal
element if it already exists.

::

    Set.insert 100 Set.empty
    > fromList [100]

    Set.insert 0 (Set.fromList [0, 2, 4, 6])
    > fromList [0,2,4,6]

Removing an element from a set
""""""""""""""""""""""""""""""

::

    Set.delete :: Ord a => a -> Set a -> Set a
    Set.delete x s = ...

Deletes the element ``x`` from the set ``s``, if it’s not a member it leaves the
set unchanged.

::

    Set.delete 0 (Set.fromList [0, 2, 4, 6])
    > fromList [2,4,6]

Filtering elements from a set
"""""""""""""""""""""""""""""

::

    Set.filter :: (a -> Bool) -> Set a -> Set a
    Set.filter predicate s = ...

Removes elements from the set ``s`` that **do not match** the ``predicate``.

::

    Set.filter (==0) (Set.fromList [0, 2, 4, 6])
    > fromList [0]


Set Operations
^^^^^^^^^^^^^^

Union
"""""

::

    Set.union :: Ord a => Set a -> Set a -> Set a
    Set.union l r = ...

Returns a set containing all elements that are in either of the two sets ``l``
or ``r`` (`set union <https://en.wikipedia.org/wiki/Union_(set_theory)>`_).

::

    Set.union Set.empty (Set.fromList [0, 2, 4, 6])
    > fromList [0,2,4,6]

    Set.union (Set.fromList [1, 3, 5, 7]) (Set.fromList [0, 2, 4, 6])
    > fromList [0,1,2,3,4,5,6,7]

Intersection
""""""""""""

::

    Set.intersection :: Ord a => Set a -> Set a -> Set a
    Set.intersection l r = ...

Returns a set the elements that are in both sets ``l`` and ``r`` (`set
intersection <https://en.wikipedia.org/wiki/Intersection_(set_theory)>`_).

::

    Set.intersection Set.empty (Set.fromList [0, 2, 4, 6])
    > fromList []

    Set.intersection (Set.fromList [1, 3, 5, 7]) (Set.fromList [0, 2, 4, 6])
    > fromList []

    Set.intersection (Set.singleton 0) (Set.fromList [0, 2, 4, 6])
    > fromList [0]

Difference
""""""""""

::

    Set.difference :: Ord a => Set a -> Set a -> Set a
    Set.difference l r = ...

Returns a set containing the elements that are in the first set ``l`` but not
the second set ``r`` (`set difference/relative compliment
<https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement>`_).

::

    Set.difference (Set.fromList [0, 2, 4, 6]) Set.empty
    > fromList [0,2,4,6]

    Set.difference (Set.fromList [0, 2, 4, 6]) (Set.fromList [1, 3, 5, 7])
    > fromList [0,2,4,6]

    Set.difference (Set.fromList [0, 2, 4, 6]) (Set.singleton 0)
    > fromList [2,4,6]

Subset
""""""

::

    Set.isSubsetOf :: Ord a => Set a -> Set a -> Bool
    Set.isSubsetOf l r = ...

Returns ``True`` if all elements in the first set ``l`` are also in the second
set ``r`` (`subset <https://en.wikipedia.org/wiki/Subset>`_).

.. NOTE::
   We use `infix notation
   <https://wiki.haskell.org/Infix_operator#Using_infix_functions_with_prefix_notation>`_
   so that it reads nicer. These are back-ticks (`), not single quotes (').

::

    Set.empty `Set.isSubsetOf` Set.empty
    > True

    Set.empty `Set.isSubsetOf` (Set.fromList [0, 2, 4, 6])
    > True

    (Set.singleton 0) `Set.isSubsetOf` (Set.fromList [0, 2, 4, 6])
    > True

    (Set.singleton 1) `Set.isSubsetOf` (Set.fromList [0, 2, 4, 6])
    > False


Debugging
---------

For dubugging you can use the ``Show`` instance which displays the elements of
the set (which we've been using all along)::

    show (Set.fromList [1,2,3])
    > "fromList [1,2,3]"

If you need to see the structure of the tree (you probably don't but if you're
curious) you can use `Set.showTree
<http://hackage.haskell.org/package/containers/docs/Data-Set.html#v:showTree>`_.

::

    putStrLn (Set.showTree (Set.fromList [1,2,3]))
    > 2
    > +--1
    > +--3


Typeclass Instances
-------------------

``Set`` is an instance of a number of common typeclasses, for the full list see
the `docs
<http://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-Set.html#t:Set>`_.

.. NOTE::
   Some constraints have been left out for brevity, and the types given below
   are speciliazed to ``Set``; the true types are more general.

- `Show
  <http://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#t:Show>`_ -
  conversion to string: ``show :: Set a -> String``
- `Eq
  <http://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#t:Eq>`_ -
  equality check: ``(==) :: Eq a => Set a -> Set a -> Bool``
- `Ord
  <http://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#t:Ord>`_ -
  comparison: ``(<) :: Ord a => Set a -> Set a -> Bool``
- `Foldable <https://wiki.haskell.org/Typeclassopedia#Foldable>`_ - collapse
  into summary value: ``foldr :: (a -> b -> b) -> b -> Set a -> b``
- `Semigroup <https://wiki.haskell.org/Typeclassopedia#Semigroup>`_ - combine
  two things together (`union`_): ``(<>) :: Set a -> Set a -> Seq a``
- `Monoid <https://wiki.haskell.org/Typeclassopedia#Monoid>`_  - a semigroup
  with an identity element: ``mempty :: Seq a``


Serialization
-------------

The best way to serialize and deserialize sets is to use one of the many
libraries which already supports serializing sets. `binary
<https://hackage.haskell.org/package/binary>`_, `cereal
<https://hackage.haskell.org/package/cereal>`_, and `store
<https://hackage.haskell.org/package/store>`_ are some common libraries that
people use.

.. TIP::
   If you are writing custom serialization code use `fromDistinctAscList
   <http://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-Set.html#v:fromDistinctAscList>`_
   (see `#405 <https://github.com/haskell/containers/issues/405>`_ for more
   info).

Performance
-----------

The API docs are annotated with the Big-*O* complexities of each of the set
operations. For benchmarks see the `haskell-perf/sets
<https://github.com/haskell-perf/sets>`_ page.


Looking for more?
-----------------

Didn't find what you're looking for? This tutorial only covered the most common
set functions, for a full list of functions see the `Set
<http://hackage.haskell.org/package/containers/docs/Data-Set.html>`_ and `IntSet
<http://hackage.haskell.org/package/containers/docs/Data-IntSet.html>`_ API
documentation.
