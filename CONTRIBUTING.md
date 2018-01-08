# Instructions for Contributors

As mentioned in the [README](https://github.com/haskell/containers/blob/master/README.md), for reporting bugs (and maybe even the respective fix), please use the [GitHub issue tracker](https://github.com/haskell/containers/issues).

For proposing API changes/enhancements, please follow the [guidelines outlined on the Haskell Wiki](https://wiki.haskell.org/Library_submissions#Guide_to_proposers). Especially note that all API changes/enhancements should be discussed on libraries@haskell.org mailing list.

## Building, testing, and benchmarking

Building, testing, and benchmarking the containers package can be done using either `cabal-install` or `stack`.

### With cabal-install

Minimum cabal version: 1.24

_Note: The procedure here is a little weird because cabal configure is unable to solve for the constraints. We're looking into why that is ([#462](https://github.com/haskell/containers/issues/462))._

```
cabal sandbox init
cabal install --only-dependencies
# Install test dependencies
cabal install 'test-framework >= 0.3.3' 'test-framework-quickcheck2 >= 0.2.9' 'QuickCheck >= 2.4.0.1' 'ChasingBottoms' 'HUnit' 'test-framework-hunit'
# Install benchmark dependencies
cabal install 'criterion'
# If you only need tests or benchmarks, you can omit the other --enable-xyz flag.
cabal configure -v2 --enable-tests --enable-benchmarks
cabal build
cabal test
cabal bench
``` 


### With [Stack](https://docs.haskellstack.org/en/stable/README/)

Minimum stack version: 1.6.1

```
stack build
stack test
stack bench
```

## Troubleshooting

- If you're using Stack, make sure you have version >= 1.6.1 ([1], [2])


[1] https://github.com/commercialhaskell/stack/issues/3624

[2] https://github.com/commercialhaskell/stack/issues/3345
