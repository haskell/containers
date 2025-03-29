# Instructions for Contributors

To report bugs, or propose enchancements or changes, please use the
[GitHub issue tracker](https://github.com/haskell/containers/issues).

You are also welcome to propose code changes in the form of
[pull requests](https://github.com/haskell/containers/pulls).
A pull request will usually aim to fix an issue. Consider creating an issue for
your change if one doesn't exist already.

## Building, testing, and benchmarking

Building, testing, and benchmarking the containers package is done using `cabal`.

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

For quicker test and benchmark build times, remove `containers` from the
`packages` stanza of the `cabal.project` file. This will avoid recompiling
some test and benchmark dependencies that depend on `containers`.

#### For Windows users

To compile `containers-tests`, you need symbolic links to be activated on git.
To do so on Windows 10 or higher, follow these steps:

1. Activate developer mode in your Windows preferences.
2. Enable git symlinks: `git config --global core.symlinks true`.
3. Clone the repository again once git is properly configured.

## Sending Pull Requests

When you send a pull request, please:

- Mention the issue you are attempting to fix.

- If your change adds new functionality or affects behaviour, please add
  QuickCheck properties exercising the code if they do not already exist. If you
  are fixing a bug that occurs too rarely for QuickCheck to hit reliably then
  consider adding unit tests as well.

- If your change is likely to affect performance, please share the results of
  running the benchmarks before and after. If the current benchmarks cannot
  demonstrate a desired difference, please try to add one or more new benchmarks
  to do so. If there are significant changes, please include the benchmark
  results in your commit message.

- When exporting a new definition, add a `@since FIXME` to the Haddocks for that
  definition. This is a [@since annotation](https://haskell-haddock.readthedocs.io/latest/markup.html#since)
  that will be updated to the next version by a maintainer before releasing it.

- (Optional) Update the [changelog](/containers/changelog.md) for non-trivial
  changes. If you don't, a maintainer will do so before the next release. You
  may choose how you wish to be credited. We will mention your name on GitHub
  unless you specify otherwise.

## Docs

The API documentation is generated using Haddock which is invoked with
`cabal haddock`.

The "user's manual" is served by ReadTheDocs at
https://haskell-containers.readthedocs.io and live in the `docs/` directory. To
build it locally run `pip install sphinx sphinx-autobuild sphinx_rtd_theme` to
install the dependencies, `git submodule update --init`, and then
`cd containers/docs/ && make html`.
