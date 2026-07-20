# hpqtypes-2.0.0.0 (????-??-??)
* Drop support for GHC < 9.6.
* Drop support for `aeson` < 2.0.
* Remove the bundled `libpqtypes` C library. The library now executes queries
  with the asynchronous API of plain `libpq` and handles the binary transport
  format with the `postgresql-binary` package.
* Replace per-type format strings with type Oids: the `pqFormat`, `pqFormat0`
  and `pqVariables` methods of `PQFormat` are replaced by `pqOid` and
  `pqArrayOid` (constants for built-in types are available in
  `Database.PostgreSQL.PQTypes.Internal.Oid`).
* Change the type of `toSQL` to `t -> Maybe Encoding`, where `Encoding` comes
  from `postgresql-binary` and `Nothing` represents NULL. The `PQDest` type
  family and `ParamAllocator` are gone.
* Change the type of `fromSQL` to `RowDecoder t`, a monadic parser that consumes
  consecutive fields of a query result. Decoders of compound results are built
  by composing decoders of their fields; the `PQBase` type family is gone.
* Remove the `PQFormat` superclass of `FromSQL`.
* Remove the `FromRow` class. Row fetching functions (`foldrDB`, `foldlDB`,
  `mapDB_`, `fetchMany`, `fetchMaybe`, `fetchOne`) now take an explicit
  `RowDecoder` of a row, e.g. `fetchMany ((,) <$> fromSQL <*> fromSQL)`.
* Row fetching functions no longer validate the width of the query result up
  front, as the number of fields a `RowDecoder` consumes is not known
  statically. A decoder that doesn't match the shape of the result throws
  `RowLengthMismatch` when a row is decoded, which in particular means that
  fetching from a result with no rows always succeeds.
* Reduce the maximum arity of tuples with a `ToRow` instance from 50 to 10.
  Larger sets of query parameters can be passed by combining rows with the
  `:++:` type (previously known as `:*:`, renamed to avoid conflicting with
  `GHC.Generics`).
* Remove the parameter of `QueryResult` along with its `Functor` and `Foldable`
  instances (superseded by row fetching functions taking a `RowDecoder`).
* Remove support for encoding of composite types along with the `Composite`,
  `CompositeRow`, `CompositeFromSQL`, `CompositeToSQL`, `CompositeArray1` and
  `CompositeArray2` machinery. Composite types are decoded with the
  `decodeComposite` combinator and no longer need to be registered (the
  `csComposites` field of `ConnectionSettings` is gone). Also, decoding of
  anonymous composite types is now supported.
* Add `genericDecoder`, which decodes consecutive fields of a row into a
  product type with a `Generic` instance (e.g. a whole row into a tuple).
  Combined with `decodeComposite`, it gives generic `FromSQL` instances for
  product types corresponding to composite types.
* Add `PQFormat`, `ToSQL` and `FromSQL` instances for lists and `Vector`s
  that encode and decode PostgreSQL arrays, so arrays of any type with the
  relevant instances can be passed as query parameters and fetched with
  plain `fromSQL`. Multi-dimensional arrays are represented by nesting
  (mixed nesting of lists and `Vector`s works too). `String` keeps
  corresponding to `text` and `[String]` to an array of `text`. The
  `Database.PostgreSQL.PQTypes.Array` module is gone.
* Remove `ArrayItemError`; errors of array element decoders report the
  position of the offending element as the column of their `ConversionError`
  instead.
* Remove `QueryError`, which was thrown when query execution produced no
  `PGresult` (e.g. because the connection died while the query was being
  sent). Such failures now throw `LibPQError`.
* The `Show` instances of `HPQTypesError` and `LibPQError` are now derived,
  like those of the other error types: they print e.g.
  `HPQTypesError "reason"` instead of `HPQTypesError (PostgreSQL): reason`.
* Add the `Database.PostgreSQL.PQTypes.Enum` module with `SQLEnum` and
  `SQLEnumAsText` deriving-via helpers for mapping Haskell enumeration types
  to values of PostgreSQL enum (or other) types. Scalar parameters work
  against both text and enum columns without casts; arrays are sent as
  `text[]` and need a cast against enum columns (e.g.
  `... = ANY($1::my_enum_type[])`).
* Add `FromSQL` and `ToSQL` instances for `Integer` and `Scientific` (mapped
  to `numeric`), `Word16`, `Word32` and `Word64`, plus a `ToSQL` instance
  for `Word` (for symmetry with `Int`).
* Add `FromSQL` and `ToSQL` instances for `IPRange` (from the `iproute`
  package), mapped to the `inet` type.
* Add support for range types: `FromSQL` and `ToSQL` instances for `Range`
  of `Int32`, `Int64`, `Scientific`, `Day`, `LocalTime` and `UTCTime`, mapped
  to `int4range`, `int8range`, `numrange`, `daterange`, `tsrange` and
  `tstzrange` respectively. The `Range` and `Bound` types from
  `postgresql-binary` are re-exported from `Database.PostgreSQL.PQTypes`.
* Change the representation of `Interval` to mirror the wire format: three
  components (microseconds, days, months) of mutually independent duration.
  The type is now opaque; values are constructed with the `iyears`,
  `imonths`, `idays`, `ihours`, `iminutes`, `iseconds` and `imicroseconds`
  functions combined via the `Monoid` instance (the sub-day functions now
  take `Int64`). The `Eq` and `Ord` instances compare the same way the
  comparison operators of the server do, i.e. by the estimate with months
  converted at 30 days and days at 24 hours. The `Show` instance shows the
  components of the wire format instead of pretty-printing.
* Executing a `COPY` statement now throws an error saying it's not supported.
  Previously it silently reported success while leaving the connection in a
  copy mode, breaking its subsequent uses.
* Cancellation of a query whose execution was interrupted by an exception now
  uses the new cancellation API when built against `libpq` >= 17, so the
  cancellation request is sent over an encrypted connection if the main
  connection is encrypted.
* Fix a bug in `connect` where interrupting it with an asynchronous exception
  could lead to a use-after-free of the buffer holding the connection string.
* Fix a bug in `changeAcquisitionModeTo` that led to holding on to an invalid
  connection object when committing a transaction during transition from the
  `AcquireAndHold` to `AcquireOnDemand` mode failed.
* Fix a bug in `withCursor` where an exception thrown from the continuation was
  masked by the failure of the subsequent cursor cleanup if the enclosing
  transaction was in the aborted state (in particular, this prevented restarts
  of transactions run with a `RestartPredicate`).
* Fix a bug in `commit`, `rollback` and `unsafeWithoutTransaction` where a
  failure of the issued `COMMIT` (e.g. due to a deferred constraint violation)
  left the session in the autocommit mode instead of starting a new
  transaction.
* Fix a bug where connection finalization interrupted with an asynchronous
  exception while another thread was using the connection permanently
  deadlocked the other thread.
* Fix transaction restart handling: a restarted transaction no longer runs
  with asynchronous exceptions masked, asynchronous exceptions (e.g. timeouts)
  no longer trigger a restart even if the restart predicate matches them, and
  if a transaction fails, a subsequent failure of its cleanup no longer masks
  the original exception (in particular hiding it from the restart predicate).

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
