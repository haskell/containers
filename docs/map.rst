Maps
====

.. highlight:: haskell

Maps (sometimes referred to as dictionaries in other languages) allow you to
store associations between *unique keys* and *values*. There are three
implementations provided by the ``containers`` package:
:haddock:`containers/Data.Map.Strict`, :haddock:`Data.Map.Lazy`, and
:haddock:`Data.IntMap`. You almost never want the lazy version so use
``Data.Map.Strict``, or if your keys are ``Int`` use ``Data.IntMap``; all of
these implementations are *immutable*.

::

    data Map k v = ...

    data IntMap v = ...

.. IMPORTANT::
   ``Map`` relies on the key type ``k`` having instances of the ``Eq`` and
   ``Ord`` typeclass for its internal representation. These are already defined
   for builtin types, and if you are using your own data type you can use the
   `deriving
   <https://en.wikibooks.org/wiki/Haskell/Classes_and_types#Deriving>`_
   mechanism.


Short Example
-------------

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


.. TIP:: You can use the `OverloadedLists
	 <https://ghc.haskell.org/trac/ghc/wiki/OverloadedLists>`_ extension so
	 you don't need to write ``fromList [1, 2, 3]`` everywhere; instead you
	 can just write ``[1, 2, 3]`` and if the function is expecting a map it
	 will be converted automatically! The code here will continue to use
	 ``fromList`` for clarity though.


Importing Map and IntMap
------------------------

When using ``Map`` or ``IntMap`` in a Haskell source file you should always use
a ``qualified`` import because these modules export names that clash with the
standard Prelude (you can import the type constructor on its own though!).

::

    import Data.Map.Strict (Map)
    import qualified Data.Map.Strict as Map

    import Data.IntMap (IntMap)
    import qualified Data.IntMap as IntMap


Common API Functions
--------------------

.. TIP::
   All of these functions that work for ``Map`` will also work for ``IntMap``,
   which has the key type ``k`` specialized to ``Int``. Anywhere that you
   see ``Map Int v`` you can replace it with ``IntMap v``.

.. NOTE::
   ``fromList [some,map,entries]`` is how a ``Map`` is printed.


Construction and Conversion
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an empty map
"""""""""""""""""""

::

    Map.empty :: Map k v
    Map.empty = ...

:map:`empty` creates a map without any entries.

::

    Map.empty
    > fromList []

Create a map with one entry (singleton)
"""""""""""""""""""""""""""""""""""""""

::

    Map.singleton :: k -> v -> Map k v
    Map.singleton key value = ...

:map:`singleton` creates a map with a single ``(key,value)`` entry in it.

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

:map:`fromList` creates a map containing the entries of the list ``xs`` where
the keys comes from the first entries of the pairs and the values from the
second. If the same key appears more than once then the last value is taken.

::

    Map.fromList []
    > fromList []

    Map.fromList [(1,"uno"), (1,"one"), (2,"two"), (3,"three")]
    > fromList [(1,"one"),(2,"two"),(3,"three")]

There's another incredibly useful function for constructing a map from a list::

    Map.fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map.Map k a
    Map.fromListWith f xs = ...

:map:`fromListWith` allows you to build a map from a list ``xs`` with repeated
keys, where ``f`` is used to "combine" (or "choose") values with the same key.

::

    -- Build a map from a list, but only keep the largest value for each key.
    Map.fromListWith max [("a", 2), ("a", 1), ("b", 2)]
    > fromList [("a",2),("b",2)]

    -- Build a histogram from a list of elements.
    Map.fromListWith (+) (map (\x -> (x, 1)) ["a", "a", "b", "c", "c", "c"])
    > fromList [("a",2),("b",1),("c",3)]

    -- Build a map from a list, combining the string values for the same key.
    Map.fromListWith (++) [(1, "a"), (1, "b"), (2, "x"), (2, "y")]
    > fromList [(1,"ba"),(2,"yx")]



Create a list from a map
""""""""""""""""""""""""

::

    Map.elems :: Map k v -> [v]
    Map.elems m = ...

:map:`elems` returns a list of values held in the map ``m``.

::

    Map.toAscList, Map.toList, Map.assocs :: Map k v -> [(k, v)]
    Map.toAscList m = ...

.. NOTE::
   These all do the same thing; use ``toAscList`` because its name indicates
   the ordering.

.. NOTE::
   ``Map.toList`` is **not** the same as ``Foldable.toList``; the latter is
   equivalent to ``elems``.

:map:`toAscList`, :map:`toList`, and :map:`assocs` returns a list containing the
(key, value) pairs in the map ``m`` in *ascending* key order.

::

    Map.toDescList :: Map k v -> [(k, v)]
    Map.toDescList m = ...

:map:`toDescList` returns a list containing the (key, value) pairs in the map
``m`` in *descending* key order.

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

:map:`null` returns ``True`` if the map ``m`` is empty, ``False`` otherwise.

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

:map:`size` returns the number of entries in the map ``m``.

::

    Map.size Map.empty
    > 0

    Map.size (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > 3

Lookup an entry in the map (lookup)
"""""""""""""""""""""""""""""""""""

::

    Map.lookup :: Ord k => k -> Map k v -> Maybe v
    Map.lookup key m = ...

    Map.!? :: Ord k => Map k v -> k -> Maybe v
    Map.!? m key = ...

:map:`lookup` the value corresponding to the given ``key``, returns ``Nothing``
if the key is not present; the ``!?`` operator (*since 0.5.10*) is a flipped
version of ``lookup`` and can often be imported unqualified.


If you want to provide a default value if the key doesn't exist you can do:

::

    import Data.Maybe (fromMaybe)

    -- fromMaybe :: a -> Maybe a -> a
    fromMaybe defaultValue (lookup k m)

For example::

    import Data.Map.Strict ((!?))
    import Data.Maybe (fromMaybe)

    Map.lookup 1 Map.empty
    > Nothing

    Map.lookup 1 (Map.fromList [(1,"one"),(2,"two"),(3,"three")])
    > Just "one"

    > (Map.fromList [(1,"one"),(2,"two"),(3,"three")]) !? 1
    > Just "one"

    fromMaybe "?" (Map.empty !? 1)
    > "?"

    fromMaybe "?" (Map.fromList [(1,"one"), (2,"two"), (3,"three")] !? 1)
    > "one"

.. WARNING::
   **DO NOT** Use ``Map.!``. It is partial and throws a runtime error if the key
   doesn't exist.

Find the minimum/maximum
""""""""""""""""""""""""

*Since version 0.5.9*

::

    Map.lookupMin, Map.lookupMax :: Map k v -> Maybe (k, v)
    Map.lookupMin m = ...
    Map.lookupMax m = ...

:map:`lookupMin` and :map:`lookupMax` respectively return the minimum or maximum
element of the map ``m``, or ``Nothing`` if the map is empty.

::

    Map.lookupMin Map.empty
    > Nothing

    Map.lookupMin (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > Just (1,"one")

    Map.lookupMax (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > Just (3,"three")

.. WARNING::
   **DO NOT** use ``Map.findMin`` or ``Map.findMax``. They are partial and throw
   a runtime error if the map is empty.

Modification
^^^^^^^^^^^^

Adding a new entry to a map
"""""""""""""""""""""""""""

::

    Map.insert :: Ord k => k -> v -> Map k v -> Map k v
    Map.insert key value m = ...

:map:`insert` adds the ``value`` into the map ``m`` with the given ``key``,
replacing the existing value if the key already exists.

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

:map:`delete` removes the entry with the specified ``key`` from the
map ``m``.  If the key doesn't exist it leaves the map
unchanged. Remember, maps are immutable so if you delete an entry from
a map you need to assign the new map to a new variable.

::

    Map.delete 1 Map.empty
    > Map.empty

    Map.delete 1 (Map.fromList [(1,"one"),(2,"two"),(3,"three")])
    > fromList [(2,"two"),(3,"three")]

Filtering map entries
"""""""""""""""""""""

::

    Map.filterWithKey :: (k -> v -> Bool) -> Map k v -> Map k v
    Map.filterWithKey predicate m = ...

:map:`filterWithKey` produces a map consisting of all entries of ``m`` for which
the ``predicate`` returns ``True``.

::

    let f key value = key == 2 || value == "one"
    Map.filterWithKey f (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > fromList [(1,"one"),(2,"two"]


Modifying a map entry
"""""""""""""""""""""

::

    Map.adjust :: Ord k => (v -> v) -> k -> Map k v -> Map k v
    Map.adjust f key m = ...

:map:`abjust` applies the value transformation function ``f`` to the entry with
given ``key``. If no entry for that key exists then the map is left unchanged.

::

    Map.alter :: Ord k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
    Map.alter f key m = ...

Apply the value transformation function ``f`` to the entry with given ``key``,
if no entry for that key exists then the function is passed ``Nothing``. If the
function returns ``Nothing`` then the entry is deleted, if the function returns
``Just v2`` then the value for the ``key`` is updated to ``v2``. In other words,
alter can be used to insert, update, or delete a value.

::

    let removeElement _ = Nothing
    Map.alter removeElement "key" (Map.fromList [("key", 0)])
    > fromList []

    let setValueToOne _ = Just 1
    Map.alter setValueToOne "key" (Map.fromList [("key", 0)])
    > fromList [("key",1)]

    Map.alter setValueToOne "key" Map.empty
    > fromList [("key",1)]

Modifying all map entries (mapping)
"""""""""""""""""""""""""""""""""""

::

    Map.map :: (a -> b) -> Map k a -> Map k v
    Map.map f m = ...

:map:`map` creates a new map by applying the transformation function ``f`` to
each entries value. This is how `Functor
<https://wiki.haskell.org/Typeclassopedia#Functor>`_ is defined for maps.

::

    Map.map (*10) (Map.fromList [("haskell", 45), ("idris", 15)])
    > fromList [("haskell",450),("idris",150)]

    -- Use the Functor instance for Map.
    (*10) <$> Map.fromList [("haskell", 45), ("idris", 15)]
    > fromList [("haskell",450),("idris",150)]

There are several other more complex mapping functions available that let you
look at other parts of the entry (such as they key) when transforming the
value. For the full list see the :haddock:`containers/Data.Map.Strict` API
documentation.


Set-like Operations
^^^^^^^^^^^^^^^^^^^

.. _union:

Union
"""""

::

    Map.unionWith :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
    Map.union f l r = ...

:map:`union` returns a map containing all entries that are keyed in either of
the two maps. If the same key appears in both maps, the value is determined by
calling ``f`` passing in the left and right value (`set union
<https://en.wikipedia.org/wiki/Union_(set_theory)>`_).

::


    Map.unionWith (++) Map.empty (Map.fromList [(1,"x"),(2,"y")])
    > fromList [(1,"x"),(2,"y")]

    let f lv rv = lv
    Map.unionWith f (Map.fromList [(1, "a")]) (Map.fromList [(1,"x"),(2,"y")])
    > fromList [(1,"a"),(2,"y")]

    Map.unionWith (++) (Map.fromList [(1, "a")]) (Map.fromList [(1,"x"),(2,"y")])
    > fromList [(1,"ax"),(2,"y")]


Intersection
""""""""""""

::

    Map.intersectionWith :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
    Map.intersection f l r = ...

:map:`intersection` returns a map containing all entries that have a key in both
maps ``l`` and ``r``. The value in the returned map is determined by calling
``f`` on the values from the left and right map (`set intersection
<https://en.wikipedia.org/wiki/Intersection_(set_theory)>`_).

::

    Map.intersectionWith (++) Map.empty (Map.fromList [(1,"x"), (2,"y")])
    > fromList []

    Map.intersectionWith (++) (Map.fromList [(1, "a")]) (Map.fromList [(1,"x"),(2,"y")])
    > fromList [(1,"ax")]



Difference
""""""""""

::

    Map.difference :: Ord k => Map k v -> Map k v -> Map k v
    Map.difference l r = ...

:map:`difference` returns a map containing all entries that have a key in the
``l`` map but not the ``r`` map (`set difference/relative complement
<https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement>`_).

::

    Map.difference (Map.fromList [(1,"one"), (2,"two"), (3,"three")]) Map.empty
    > fromList [(1,"uno"),(2,"two"),(3,"three")]

    Map.difference (Map.fromList[(1,"one"), (2,"two")]) (Map.fromList [(1,"uno")])
    > fromList [(2,"two")]


Serialization
-------------

The best way to serialize and deserialize maps is to use one of the many
libraries which already support serializing maps. :haddock:`binary`,
:haddock:`cereal`, and :haddock:`store` are some common libraries that people
use.

.. TIP::
   If you are writing custom serialization code use :map:`fromDistinctAscList`
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
map functions, for a full list of functions see the :map:`Data.Map.Strict` and
:map:`Data.IntMap` API documentation.
