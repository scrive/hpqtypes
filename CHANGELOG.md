# hpqtypes-1.14.0.0 (2025-12-10)
* Make `begin`, `commit` and `rollback` do nothing instead of throwing an error
  if the on demand connection acquisition mode is active.

# hpqtypes-1.13.0.1 (2025-11-27)
* Fix a bug in `initConnectionState` and `finalizeConnectionState` that could
  lead to leaking connections.

# hpqtypes-1.13.0.0 (2025-11-26)
* Drop support for GHC < 9.2.
* Include time spent executing queries in `ConnectionStats`.
* Add `initialConnectionStats`.
* Introduce on-demand connection acquisition mode.

# hpqtypes-1.12.0.0 (2024-03-18)
* Drop support for GHC 8.8.
* Attach `CallStack` and `BackendPid` to `DBException`.
* Add `getBackendPid` to `MonadDB` for getting the ID of the server process
  attached to the current session.

# hpqtypes-1.11.1.2 (2023-11-08)
* Support multihost setups and the `connect_timeout` parameter in the connection
  string.

# hpqtypes-1.11.1.1 (2023-03-14)
* Add support for GHC 9.6.

# hpqtypes-1.11.1.0 (2023-01-31)
* Add support for setting a custom role when establishing a connection.

# hpqtypes-1.11.0.0 (2023-01-18)
* Require `resource-pool` >= 0.4 and adjust the `createPool` function to
  seamlessly accommodate future changes to the `resource-pool` library.

# hpqtypes-1.10.0.2 (2022-10-12)
* Simplify Setup.

# hpqtypes-1.10.0.1 (2022-09-23)
* Provide detailed error when `openConnection` fails.

# hpqtypes-1.10.0.0 (2022-09-14)
* Improve `Show` instances of `HPQTypesError` and `LibPQError`.
* Remove `INLINE` and `INLINABLE` pragmas.
* Fix a rare bug in the `connect` function related to file descriptors.
* Add support for GHC 9.4.
* Require `resource-pool >= 0.3` (changes type signature of `poolSource`).

# hpqtypes-1.9.4.0 (2022-05-18)
* Add support for prepared statements.
* Make more foreign C calls safe.
* Don't manage `PGconn` with a `ForeignPtr`.
* Use `closeFdWith` when closing connections.

# hpqtypes-1.9.3.1 (2022-03-30)
* Fix `withTransaction` and `withSavepoint` with short-circuiting monad
  transformers such as `ExceptT`.

# hpqtypes-1.9.3.0 (2022-02-25)
* Fix support for M1 chips.
* Add support for aeson >= 2.0.
* Add support for GHC 9.2.
* Drop support for GHC < 8.8.

# hpqtypes-1.9.2.1 (2021-11-04)
* Improve an SQL query that gets information about composite types.

# hpqtypes-1.9.2.0 (2021-09-29)
* Add method withFrozenLastQuery to (temporarily) stop recording queries.

# hpqtypes-1.9.1.2 (2021-07-29)
* Fix compilation issues caused by ambiguos occurence of `controlT`.

# hpqtypes-1.9.1.1 (2021-05-27)
* Support GHC 9.0.

# hpqtypes-1.9.1.0 (2020-09-14)
* Expose aesonFromSQL and aesonToSQL for convenience.

# hpqtypes-1.9.0.1 (2020-09-04)
* Remove upper bounds of dependencies.

# hpqtypes-1.9.0.0 (2020-04-02)
* Support GHC 8.8 and 8.10.

# hpqtypes-1.8.0.1 (2020-02-06)
* Make poolSource work properly with shortcircuiting monad transformers.

# hpqtypes-1.8.0.0 (2019-10-31)
* Implement `UUID` format ([#17](https://github.com/scrive/hpqtypes/pull/17)).
* Support GHC 8.8.

# hpqtypes-1.7.0.0 (2019-05-21)
* Remove the `Default` instances for `ConnectionSettings` and
  `TransactionSettings`; use `defaultConnectionSettings` and
  `defaultTransactionsettings` instead
  ([#15](https://github.com/scrive/hpqtypes/pull/15)).

# hpqtypes-1.6.1.0 (2018-11-24)
* Add support for cursors
  ([#13](https://github.com/scrive/hpqtypes/pull/13)).
* Remove explicit `deriving Typeable` from all data types.

# hpqtypes-1.6.0.0 (2018-07-11)
* Convert the `PQFormat` class to use `TypeApplications` instead of an
  `undefined :: t` argument
  ([#11](https://github.com/scrive/hpqtypes/pull/11)).
* Support GHC 8.6.
* Drop support for GHC < 8.

# hpqtypes-1.5.3.0 (2018-06-04)
* Add INLINE/INLINEABLE pragmas for call site specialization.
* Remove -O2 -funbox-strict-fields from ghc-options.
* Make query execution interruptible with asynchronous exceptions.
* Make connect interruptible with asynchronous exceptions.

# hpqtypes-1.5.2.0 (2018-03-18)
* Support GHC 8.4.1.

# hpqtypes-1.5.1.1 (2016-09-22)
* Fix test suite compilation with GHC 8.
* Fix lower bound of base version.
* Fix compilation with 'cabal new-build' and Cabal < 1.24.

# hpqtypes-1.5.1 (2016-07-04)
* Do not use linux/limits.h.

# hpqtypes-1.5.0 (2016-06-21)
* Remove orphan MonadDB instances.
* Turn ConnectionSource into indexed datatype.
* Remove Binary wrapper and (de)serialize ByteString as bytes.
* Use Text instead of ByteString where appropriate.
* Use UTF-8 client encoding by default for compatibility with Text.

# hpqtypes-1.4.5 (2016-05-30)
* Fix compilation with Cabal 1.24 and GHC 8.0.1.

# hpqtypes-1.4.4 (2016-01-19)
* Fix lower bound of base version.

# hpqtypes-1.4.3 (2015-10-09)
* Remove invalid FromSQL ZonedTime instance.

# hpqtypes-1.4.2 (2015-06-08)
* Use strict StateT for DBT.
* Use catch in withTransaction only if it might be used.

# hpqtypes-1.4.1 (2015-05-15)
* Add support for json and jsonb sql types.
* Add support for lazy ByteString and Text.

# hpqtypes-1.4.0 (2015-02-26)
* Add support for QuickCheck 2.7.
* Add support for notifications.
* Remove SpaceMonoid, use Monoid and IsString instead.
* Use data-default-class package for default values.
* Drop Single, use Identity functor instead.
* Remove someSQL from IsSQL class.
* Remove foldlM/foldrM from MonadDB and make QueryResult instance of Foldable instead.
* Add support for a type representing cartesian product of rows for more composability.
* Do not wrap exceptions thrown from DBT in DBException unless explicitly requested.
* Provide custom Show instance for Interval.
* Add ToSQL instance for Int.

# hpqtypes-1.3.2 (2015-01-27)
* Replace wrong package uploaded to hackage.

# hpqtypes-1.3.1 (2015-01-26)
* Add support for XML type.

# hpqtypes-1.3.0 (2015-01-09)
* Composite: make {from,to}Composite functions pure.

# hpqtypes-1.2.5 (2015-01-04)
* Add support for monad-control >= 1.0.0.1.

# hpqtypes-1.2.4 (2014-12-08)
* Add IsString instance for Savepoint newtype.
