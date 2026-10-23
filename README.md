# hpqtypes

[![CI](https://github.com/scrive/hpqtypes/actions/workflows/haskell-gha.yaml/badge.svg?branch=master)](https://github.com/scrive/hpqtypes/actions/workflows/haskell-gha.yaml)
[![Hackage](https://img.shields.io/hackage/v/hpqtypes.svg)](https://hackage.haskell.org/package/hpqtypes)
[![Stackage LTS](https://www.stackage.org/package/hpqtypes/badge/lts)](https://www.stackage.org/lts/package/hpqtypes)
[![Stackage Nightly](https://www.stackage.org/package/hpqtypes/badge/nightly)](https://www.stackage.org/nightly/package/hpqtypes)

Efficient and easy-to-use bindings to (slightly modified) libpqtypes,
libpq extension that adds support for binary transport format and
composite types.

Source code of libpqtypes is bundled along with the bindings.

Examples can be found in the [examples](https://github.com/scrive/hpqtypes/tree/master/examples) directory.

## Benchmarks

`hpqtypes-opentelemetry` has benchmarks of the query summary that it extracts
for each span. Run them with:

```
cabal bench hpqtypes-opentelemetry -O2
```

To check a change for regressions, save a baseline before it and compare
against it afterwards:

```
cabal bench hpqtypes-opentelemetry -O2 --benchmark-options='--csv baseline.csv'
cabal bench hpqtypes-opentelemetry -O2 --benchmark-options='--baseline baseline.csv --fail-if-slower 20'
```

The timings vary between runs by tens of percent depending on the load of the
machine, so run both on an otherwise idle machine and pick a threshold above
that noise. Use `-p <pattern>` in `--benchmark-options` to run only some of the
benchmarks, e.g. `-p union`.
