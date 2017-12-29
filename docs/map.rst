Maps
====

.. highlight:: haskell

Maps, sometimes referred to as dictionaries in other languages, allow you to
store associations between keys and values. There are three implementations
provided by the ``containers`` package: `Data.Map.Strict
<http://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html>`_,
`Data.Map.Lazy
<http://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html>`_, and
`Data.IntMap
<http://hackage.haskell.org/package/containers/docs/Data-IntMap.html>`_. You
almost never want the lazy version so use ``Data.Map.Strict``, or if your keys
are ``Int`` use ``Data.IntMap``.


Example
-------

The following GHCi session shows some of the basic map functionality::

    import qualified Data.Map.Strict as Map

    let nums = Map.fromList [(1,"one"), (2,"two"), (3,"three")]

    -- Check if we know the english words for 1 and 4
    Map.member nums 1
    > True

    Map.member nums 4
    > False


    -- Get the english word for the number 3 and 4.
    Map.lookup 3 nums
    > Just "three"

    Map.lookup 4 nums
    > Nothing


    -- Add (4, "four") to our original map.
    let moreNums = Map.insert 4 "four" nums

    Map.member moreNums 4
    > True


    -- Remove the entry for 1 from our original map.
    let fewerNums = Map.delete 1 nums

    Map.toAscList fewerNums
    > [(2,"two"),(3,"three")]


    -- Create a new map and combine it with our original map.
    -- Note that these operations are left-biased.
    let newNums = Map.fromList [(3,"new three"), (4,"new four")]

    Map.union newNums nums
    > fromList [(1,"one"),(2,"two"),(3,"new three"),(4,"new four")]


Importing Map and IntMap
------------------------

When using ``Map`` or ``IntMap`` in a Haskell source file you should always use
a ``qualified`` import because they export names that clash with the standard
Prelude (you can import the type constructor on its own though!).

::

    import Data.Map.Strict (Map)
    import qualified Data.Map.Strict as Map

    import Data.IntMap (IntMap)
    import qualified Data.IntMap as IntMap


Common API Functions
--------------------

.. NOTE::
   All of these functions that work for ``Map`` will also work for ``IntMap``,
   which has the key type ``k`` specialized to ``Int``. Anywhere that you
   see ``Map k v`` you can replace it with ``IntMap v``.

.. NOTE::
   ``fromList [some,map,entries]`` is how a ``Map`` is printed.


Construction and Conversion
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an empty map
"""""""""""""""""""

::

    Map.empty :: Map k v
    Map.empty = ...

Creates a map with zero entries.

::

    Map.empty
    > fromList []

Create a map with one entry (singleton)
"""""""""""""""""""""""""""""""""""""""

::

    Map.singleton :: k -> v -> Map k v
    Map.singleton key value = ...

Creates a map with a single ``(key,value)`` entry in it.

::

    Map.singleton 1 "one"
    > fromList [(1,"one")]

    Map.singleton "containers" ["base"]
    > fromList [("containers",["base"])]

Create a map from a list
""""""""""""""""""""""""

::

    Map.fromList :: Ord k => [(k, v)] -> Map k v
    Map.fromList xs = ...

Creates a map containing the entries of the list ``xs`` where the keys comes
from the first entries of the pairs and the values from the second. If the same
key appears more than once then the last value is taken.

::

    Map.fromList []
    > fromList []

    Map.fromList [(1,"uno"), (1,"one"), (2,"two"), (3,"three")]
    > fromList [(1,"one"),(2,"two"),(3,"three")]

Create a list from a map
""""""""""""""""""""""""

::

    Map.elems :: Map k v -> [v]
    Map.elems m = ...

Returns a list of values held in the map ``m``.

::

    Map.toAscList, Map.toList, Map.assocs :: Map k v -> [(k, v)]
    Map.toAscList m = ...

Returns a list containing the (key, value) pairts in the map ``m`` in
*ascending* key order.

.. NOTE::
   These all do the same thing, use ``toAscList`` because its name indicates
   the ordering.

::

    Map.toDescList :: Map k v -> [(k, v)]
    Map.toDescList m = ...

Returns a list containing the (key, value) pairs in the map ``m`` in
*descending* key order.

::

    Map.elems (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > ["one","two","three"]

    Map.toAscList (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > [(1,"one"),(2,"two"),(3,"three")]

    Map.toDescList (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > [(3,"three"),(2,"two"),(1,"one")]


Querying
^^^^^^^^

Check if a map is empty
"""""""""""""""""""""""

::

    Map.null :: Map k v -> Bool
    Map.null m = ...

Returns ``True`` if the map ``m`` is empty, ``False`` otherwise.

::

    Map.null Map.empty
    > True

    Map.null (Map.fromList [(1,"one")])
    > False

The number of entries in a map
""""""""""""""""""""""""""""""

::

    Map.size :: Map k v -> Int
    Map.size m = ...

Returns the number of entries in the map ``m``.

::

    Map.size Map.empty
    > 0

    Map.size (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > 3


Check if a key is present in the map (member)
"""""""""""""""""""""""""""""""""""""""""""""

::

    Map.member :: Ord k => k -> Map k v -> Bool
    Map.member key m = ...

Returns ``True`` if the ``key`` is in the map ``m``, ``False`` otherwise.

::

    Map.member 1 Map.empty
    > False

    Map.member 1 (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > True

Lookup an entry in the map (lookup)
"""""""""""""""""""""""""""""""""""

::

    Map.lookup :: Ord k => k -> Map k v -> Maybe v
    Map.lookup key m = ...

Lookup the value corresponding to the given ``key``, returns ``Nothing`` if the
key is not present.

::

    Map.findWithDefault :: Ord k => v -> k -> Map k v -> v
    Map.findWithDefault defaultValue key m = ...

Lookup the value corresponding to the given ``key`` in the map ``m``, return the
``defaultValue`` if the key is not present.

::

    Map.lookup 1 Map.empty
    > Nothing

    Map.lookup 1 (Map.fromList [(1,"one"),(2,"two"),(3,"three")])
    > Just "one"

    Map.findWithDefault "?" 1 Map.empty
    > "?"

    Map.findWithDefault "?" 1 (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > "one"

.. WARNING::
   **DO NOT** Use ``Map.!``, it is partial and throws a runtime error if the key
   doesn't exist.

Find the minimum/maximum
""""""""""""""""""""""""

*Since version 0.5.9*

::

    Map.lookupMin, Map.lookupMax :: Map k v -> Maybe (k, v)
    Map.lookupMin m = ...
    Map.lookupMax m = ...

Return the minimum, or maximum respectively, element of the map ``m``, or
``Nothing`` if the map is empty.

::

    Map.lookupMin Map.empty
    > Nothing

    Map.lookupMin (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > Just (1,"one")

    Map.lookupMax (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > Just (3,"three")

.. WARNING::
   **DO NOT** use ``Map.findMin`` or ``Map.findMax``, they are partial and throw
   a runtime error if the map is empty.

Modification
^^^^^^^^^^^^

Adding a new entry to a map
"""""""""""""""""""""""""""

::

    Map.insert :: Ord k => k -> v -> Map k v -> Map k v
    Map.insert key value m = ...

Insert the ``value`` into the map ``m`` with the given ``key``, replacing the
existing value if the key already exists.

::

    Map.insert 1 "one" Map.empty
    > Map.fromList [(1,"one")]

    Map.insert 4 "four" (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > fromList [(1,"one"),(2,"two"),(3,"three"),(4,"four")]

    Map.insert 1 "uno" (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > fromList [(1,"uno"),(2,"two"),(3,"three")]


Removing an entry from a map
""""""""""""""""""""""""""""

::

    Map.delete :: Ord k => k -> Map k v -> Map k v
    Map.delete key m = ...

Deletes the entry with the specified ``key`` from the map ``m``, if the key
doesn't exist it leaves the map unchanged.

::

    Map.delete 1 Map.empty
    > Map.empty

    Map.delete 1 (Map.fromList [(1,"one"),(2,"two"),(3,"three")])
    > fromList [(2,"two"),(3,"three")]

Filtering map entries
"""""""""""""""""""""

::

    Map.filter :: (v -> Bool) -> Map k v -> Map k v
    Map.filter predicate m = ...

Removes entries from the map ``m`` who's values **do not match** the
``predicate``.

::

    Map.filter (=="one") (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > fromList [(1,"one")]


Set-like Operations
^^^^^^^^^^^^^^^^^^^

.. _union:

Union
"""""

::

    Map.union :: Ord k => Map k v -> Map k v -> Map k v
    Map.union l r = ...

Returns a map containing all entries that are keyed in either of the two map. If
the same key appears in both maps, the value from the left map ``l`` taken (`set
union <https://en.wikipedia.org/wiki/Union_(set_theory)>`_).

::

    Map.union Map.empty (Map.fromList [(1,"one"),(2,"two")])
    > fromList [(1,"one"),(2,"two")]

    Map.union (Map.fromList [(1, "uno")]) (Map.fromList [(1,"one"),(2,"two")])
    > fromList [(1,"uno"),(2,"two")]

Intersection
""""""""""""

::

    Map.intersection :: Ord k => Map k v -> Map k v -> Map k v
    Map.intersection l r = ...

Returns a map containing all entries that are keyed in both maps ``l`` and
``r``. The value from the left map is taken if the key exists in both maps (`set
intersection <https://en.wikipedia.org/wiki/Intersection_(set_theory)>`_).

::

    Map.intersection Map.empty (Map.fromList [(1,"one"), (2,"two")])
    > fromList []

    Map.intersection (Map.fromList [(1, "uno")]) (Map.fromList [(1,"one"),(2,"two")])
    > fromList [(1,"uno")]

Difference
""""""""""

::

    Map.difference :: Ord k => Map k v -> Map k v -> Map k v
    Map.difference l r = ...

Returns a map containing all entries that are keyed in the ``l`` map but not the
``r`` map (`set difference/relative compliment
<https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement>`_).

::

    Map.difference (Map.fromList [(1,"one"), (2,"two"), (3,"three")]) Map.empty
    > fromList [(1,"uno"),(2,"two"),(3,"three")]

    Map.difference (Map.fromList[(1,"one"), (2,"two")]) (Map.fromList [(1,"uno")])
    > fromList [(2,"two")]

Subset (submap)
"""""""""""""""

::

    Map.isSubmapOf :: (Eq a, Ord k) => Map k v -> Map k v -> Bool
    Map.isSubmapOf l r = ...

Returns ``True`` if all entries--(keys, value) pairs--in the left map ``l``
exist in the right map ``r``, ``False`` otherwise (`subset
<https://en.wikipedia.org/wiki/Subset>`_).

.. NOTE::
   We use `infix notation
   <https://wiki.haskell.org/Infix_operator#Using_infix_functions_with_prefix_notation>`_
   so that it reads nicer. These are back-ticks (`), not quotes.

::

    Map.empty `Map.isSubmapOf ` Map.empty
    > True

    Map.empty `Map.isSubmapOf` (Map.fromList [(1,"one"), (2,"two")])
    > True

    (Map.singleton 1 "uno") `Map.isSubmapOf` (Map.fromList [(1,"one"), (2,"two")])
    > True


Debugging
---------

For dubugging you can use the ``Show`` instance which displays the entries of
the map (which we've been using all along)::

    show (Map.fromList  [(1,"one"), (2,"two"), (3,"three")])
    > "fromList [(1,\"one\"),(2,\"two\"),(3,\"three\")]"

If you need to see the structure of the tree (you probably don't but if you're
curious) you can use `Map.showTree
<http://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html#v:showTree>`_.

::

    putStr (Map.showTree (Map.fromList  [(1,"one"), (2,"two"), (3,"three")]))
    > 2:="two"
    > +--1:="one"
    > +--3:="three"


Typeclass Instances
-------------------

``Map`` is an instance of a number of common typeclasses, for the full list see
the `docs
<http://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-Map-Strict.html#t:Map>`_.

.. NOTE::
   Some constraints have been left out for brevity, and the types given below
   are speciliazed to ``Map``; the true types are more general.

- `Show
  <http://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#t:Show>`_ -
  conversion to string: ``show :: Map k v -> String``
- `Eq
  <http://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#t:Eq>`_ -
  equality check: ``(==) :: Map k v -> Map k v -> Bool``
- `Ord
  <http://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#t:Ord>`_ -
  comparison: ``(<) :: Map k v -> Map k v -> Bool``
- `Foldable <https://wiki.haskell.org/Typeclassopedia#Foldable>`_ - collapse
  into summary value: ``foldr :: (v -> b -> b) -> b -> Map k v -> b``
- `Semigroup <https://wiki.haskell.org/Typeclassopedia#Semigroup>`_ - combine
  two things together (union_): ``(<>) :: Map k v -> Map k v -> Map k v``
- `Monoid <https://wiki.haskell.org/Typeclassopedia#Monoid>`_  - a semigroup
  with an identity element: ``mempty :: Map k v``
- `Functor <https://wiki.haskell.org/Typeclassopedia#Functor>`_ - a container
  that can be mapped over: ``fmap :: (v -> b) -> Map k v -> Map k b``
- `Traversable <https://wiki.haskell.org/Typeclassopedia#Traversable>`_ - a
  functor with effects, follow the link :)


Serialization
-------------

The best way to serialize and deserialize maps is to use one of the many
libraries which already supports serializing map. `binary
<https://hackage.haskell.org/package/binary>`_, `cereal
<https://hackage.haskell.org/package/cereal>`_, and `store
<https://hackage.haskell.org/package/store>`_ are some common libraries that
people use.

.. TIP::
   If you are writing custom serialization code use `fromDistinctAscList
   <http://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-Map-Strict.html#v:fromDistinctAscList>`_
   (see `#405 <https://github.com/haskell/containers/issues/405>`_ for more
   info).


Performance
-----------

The API docs are annotated with the Big-*O* complexities of each of the map
operations. For benchmarks see the `haskell-perf/dictionaries
<https://github.com/haskell-perf/dictionaries>`_ page.


Looking for more?
-----------------

Didn't find what you're looking for? This tutorial only covered the most common
map functions, for a full list of functions see the `Map
<http://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html>`_ and
`IntMap <http://hackage.haskell.org/package/containers/docs/Data-IntMap.html>`_
API documentation.
