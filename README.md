# hpqtypes

[![CI](https://github.com/scrive/hpqtypes/actions/workflows/haskell-ci.yml/badge.svg?branch=master)](https://github.com/scrive/hpqtypes/actions/workflows/haskell-ci.yml)
[![Hackage](https://img.shields.io/hackage/v/hpqtypes.svg)](https://hackage.haskell.org/package/hpqtypes)
[![Stackage LTS](https://www.stackage.org/package/hpqtypes/badge/lts)](https://www.stackage.org/lts/package/hpqtypes)
[![Stackage Nightly](https://www.stackage.org/package/hpqtypes/badge/nightly)](https://www.stackage.org/nightly/package/hpqtypes)

Efficient and easy-to-use bindings to `libpq` that use the binary
transport format for queries and their results.

Main features:

* Queries and their parameters are specified separately, so SQL injection is not
  possible by construction.
* Execution of queries can be interrupted with asynchronous exceptions (in such
  case the query is cancelled server-side as well). Note that this requires the
  threaded runtime to work correctly.
* Conversion between Haskell types and their SQL counterparts is handled by the
  `ToSQL` and `FromSQL` type classes. Rows are decoded with composable, monadic
  `RowDecoder`s that verify compatibility between SQL and Haskell types.
* Support for arrays (represented as plain lists or `Vector`s, with nesting for
  multi-dimensional ones), anonymous and user-defined composite types,
  PostgreSQL enums and NOTIFY/LISTEN.

Examples can be found in the
[examples](https://github.com/scrive/hpqtypes/tree/master/examples) directory.
