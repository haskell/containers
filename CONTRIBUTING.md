# Instructions for Contributors

To report bugs, please use the [GitHub issue tracker](https://github.com/haskell/containers/issues).
We also appreciate [pull requests](https://github.com/haskell/containers/pulls) on GitHub.

For proposing API changes or enhancements, please follow the [guidelines outlined on the Haskell Wiki](https://wiki.haskell.org/Library_submissions#Guide_to_proposers).
All such changes should be discussed on the libraries@haskell.org mailing list.


## Building, testing, and benchmarking

Building, testing, and benchmarking the containers package is done using `cabal`.

To avoid recompiling the tests' other dependencies when making changes, you can
remove `containers` from the `packages` stanza of the `cabal.project` file.
Note: this will not work in the unlikely event that you are fixing a bug that
affects the test or benchmark framework itself. The `Data.Set` (for example)
used in the tests and benchmarks is compiled separately from the one exposed by
the `containers` package.

### Procedure

Minimum cabal version: 2.4

Build:
```
cabal build containers
```

Run all tests or benchmarks:
```
cabal test containers-tests
cabal bench containers-tests
```

To run a particular test or benchmark suite, name the target from
`containers-tests/containers-tests.cabal`:
```
cabal run set-properties  # cabal test also works
cabal run map-benchmarks  # cabal bench also works
```

To run selective tests or benchmarks, you can pass a
[filter pattern](https://hackage.haskell.org/package/tasty#patterns) as
supported by `tasty`:
```
cabal run set-properties -- -p fromList
cabal test set-properties --test-options "-p fromList"
```

#### For Windows users

To compile `containers-tests`, you need symbolic links to be activated on git.
To do so on Windows 10 or higher, follow these steps:

1. Activate developer mode in your Windows preferences.
2. Enable git symlinks: `git config --global core.symlinks true`.
3. Clone the repository again once git is properly configured.

## Sending Pull Requests

When you send a pull request, please:

- Link to the libraries@haskell.org discussion thread if you are changing the
  public API.

- If you are requesting a change that is likely to affect performance, we will
  be able to evaluate it better if you include the results of running the
  benchmarks before and after. If the current benchmarks cannot demonstrate
  a desired difference, please try to add one or more new benchmarks to do so.
  If there are significant changes, please include the benchmark results in
  your commit message.

- If you are requesting a change that adds new functionality or affects
  behaviour, please add QuickCheck properties exercising the code if they
  do not already exist. If you are fixing a bug that occurs too rarely for
  QuickCheck to hit reliably then consider adding unit tests as well.
  
- Update the change log for non-trivial changes.

- Let us know how you wish to be credited in the changelog.

## Docs

The API documentation is generated using Haddock which is invoked with
`cabal haddock`.

The "user's manual" is served by ReadTheDocs at
https://haskell-containers.readthedocs.io and live in the `docs/` directory. To
build it locally run `pip install sphinx sphinx-autobuild sphinx_rtd_theme` to
install the dependencies, `git submodule update --init`, and then
`cd docs/ && make html`.
