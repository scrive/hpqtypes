# hpqtypes

[![CI](https://github.com/scrive/hpqtypes/actions/workflows/haskell-ci.yml/badge.svg?branch=master)](https://github.com/scrive/hpqtypes/actions/workflows/haskell-ci.yml)
[![Hackage](https://img.shields.io/hackage/v/hpqtypes.svg)](https://hackage.haskell.org/package/hpqtypes)
[![Stackage LTS](https://www.stackage.org/package/hpqtypes/badge/lts)](https://www.stackage.org/lts/package/hpqtypes)
[![Stackage Nightly](https://www.stackage.org/package/hpqtypes/badge/nightly)](https://www.stackage.org/nightly/package/hpqtypes)

hpqtypes provides efficient bindings to `libpq`. Queries and their results
travel in the binary transport format.

Main features:

* You write a query and its parameters separately. SQL injection is not
  possible by construction.
* An asynchronous exception interrupts a running query. The library then
  cancels the query on the server too. This feature requires the threaded
  runtime.
* The `ToSQL` and `FromSQL` type classes convert between Haskell types and
  their SQL counterparts. Composable, monadic `RowDecoder`s decode rows. A
  `RowDecoder` compares the SQL type of each column with the expected Haskell
  type.
* The library supports arrays, anonymous and user-defined composite types,
  PostgreSQL enums and NOTIFY/LISTEN. An array is a plain list or a `Vector`.
  A multi-dimensional array is a nested list or `Vector`.

The [examples](https://github.com/scrive/hpqtypes/tree/master/examples)
directory contains examples.

If you upgrade from 1.x, read the
[migration guide](https://github.com/scrive/hpqtypes/blob/master/migration-2.0.md).
