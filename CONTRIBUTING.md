# Instructions for Contributors

As mentioned in the [README](https://github.com/haskell/containers/blob/master/README.md), for reporting bugs (and maybe even the respective fix), please use the [GitHub issue tracker](https://github.com/haskell/containers/issues).

For proposing API changes/enhancements, please follow the [guidelines outlined on the Haskell Wiki](https://wiki.haskell.org/Library_submissions#Guide_to_proposers). Especially note that all API changes/enhancements should be discussed on libraries@haskell.org mailing list.

## Building the Library

Building the containers package can be done easily using either cabal-install or stack.

### With cabal-install

```
cabal sandbox init
cabal build
```

### With [Stack](https://docs.haskellstack.org/en/stable/README/)

```
stack init   # If you haven't previously initialized stack
stack build
```

## Running Tests

At the time of writing it was difficult to get cabal-install to find a compatible set of dependencies to run the tests so we recommend using [Stack](https://docs.haskellstack.org/en/stable/README/) at this point in time.

### With stack

```
stack init   # If you haven't previously initialized stack
stack test
```

## Troubleshooting

- If you're using stack, make sure you have version >= 1.6.1 ([1], [2])


[1] https://github.com/commercialhaskell/stack/issues/3624

[2] https://github.com/commercialhaskell/stack/issues/3345
