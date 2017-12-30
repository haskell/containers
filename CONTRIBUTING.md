# Instructions for Contributors

For reporting bugs (and maybe even the respective fix), please use the [GitHub issue tracker](https://github.com/haskell/containers/issues).

For proposing API changes/enhancements, please follow the [guidelines outlined on the Haskell Wiki](https://wiki.haskell.org/Library_submissions#Guide_to_proposers). Especially note that all API changes/enhancements should be discussed on libraries@haskell.org mailing list.


## Building and testing

Building and testing the containers package can be done using either `cabal-install` or `stack`.

### With cabal-install

```
cabal sandbox init
cabal install --only-dependencies
cabal install 'test-framework >= 0.3.3' 'test-framework-quickcheck2 >= 0.2.9' 'QuickCheck >= 2.4.0.1' 'ChasingBottoms' 'HUnit' 'test-framework-hunit'
cabal configure -v2 --enable-tests
cabal build
cabal test
```

### With [Stack](https://docs.haskellstack.org/en/stable/README/)

```
stack init   # If you haven't previously initialized stack
stack build
stack test
```


## Troubleshooting

- If you're using Stack, make sure you have version >= 1.6.1
  ([stack#3524](https://github.com/commercialhaskell/stack/issues/3624),
  [stack#3345](https://github.com/commercialhaskell/stack/issues/3345)).


## Sending Pull Requests

When you send a pull request, please:

- Link to libraries@haskell.org discussion thread if you are changing the public
  API.

- Run the benchmarks and include the results in the commit message and Pull
  Request.

- Add unit tests exercising any changes you make, especially if it is a bug
  fix.
