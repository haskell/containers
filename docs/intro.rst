Introduction
============

The ``containers`` package provides implementations of various immutable data
structures.

Some of the data structures provided by this package have a very large API
surface (for better or worse). The docs here focus on the most common functions
which should be more than enough to get you started. Once you know the basics,
or if you're looking for a specific function, you can head over to the
`containers hackage page <https://hackage.haskell.org/package/containers>`_ to
check out the full API documentation!

Provided Data Structures
------------------------

- :doc:`set`: ordered, non-duplicated elements
- :doc:`map`: ordered maps from keys to values (aka. dictionaries)
- :doc:`sequence`: finite sequence of elements, an efficient alternative to list

.. NOTE::
   You'll need ``containers >= 0.5.9`` for a few of the examples. See
   `Version Requirements`_ for info on how to check which version you have and
   how to upgrade.


Related Packages
----------------

- `unordered-containers
  <https://hackage.haskell.org/package/unordered-containers>`_ - containers
  using hashing instead of ordering.

- `array <https://hackage.haskell.org/package/array>`_ - mutable and immutable
  arrays.

- `bytestring <https://hackage.haskell.org/package/bytestring>`_ - compact,
  immutable bytestrings, useful for binary and 8-bit character data.

- `dlist <https://hackage.haskell.org/package/dlist>`_ - difference lists with
  *O(1)* append, useful for efficient logging and pretty printing.

- `hashtables <http://hackage.haskell.org/package/hashtables>`_ - mutable hash
  tables in the ST monad.

.. _installing:

Installing and using the ``containers`` package
-----------------------------------------------

Version Requirements
^^^^^^^^^^^^^^^^^^^^

For some of the examples you'll need ``containers >= 0.5.9`` which ships with
``GHC >= 8.2``. You can check to see which version you have installed with:

::

    ghc-pkg list | grep containers
    > containers-0.5.10.2

If you have an older version, don't worry about it, the majority of the code
works with older versions of the package. If you want, you can get version
``0.5.10`` with `Stack <https://www.haskellstack.org>`_ using ``stack --resolver
lts-10.0 ghci``.


Importing ``container`` modules
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

All of the modules in ``containers`` should be imported ``qualified`` since they
use names that conflict with the standard Prelude. See the instructions for `In
GHCi`_ below for an example.


In GHCi
^^^^^^^

Start the GHCi `REPL
<https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop>`_ with
``ghci`` or ``stack ghci``. Once the REPL is loaded all you need to do is import
the modules you want to use::

    import qualified Data.Set as Set
    import qualified Data.Map.Strict as Map
    import qualified Data.Sequence as Seq


In a `Cabal <http://cabal.readthedocs.io>`_ project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Add ``containers`` to the ``build-depends:`` stanza for your library,
executable, or test-suite::

    library
        build-depends:
	    base >= 4.3 && < 5,
	    containers >= 0.5.7 && < 0.6


In a `Stack <https://www.haskellstack.org>`_ project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Same as a Cabal project, add the dependency to the the ``build-depneds:``
stanza.

::

    library
        build-depends:
	    base,
	    containers

.. NOTE::
   You can omit the version range since Stackage snapshots have pre-defined
   versions.
