# hpqtypes-2.0.0.0 (????-??-??)
* Drop support for GHC < 9.6.
* Drop support for `aeson` < 2.0.
* Remove the bundled `libpqtypes` C library. The library now executes queries
  with plain `libpq` and handles the binary transport format itself. The
  encoders and decoders are derived from the `postgresql-binary` package.
* Replace per-type format strings with type Oids. The `pqOid` and
  `pqArrayOid` methods of `PQFormat` replace `pqFormat`, `pqFormat0` and
  `pqVariables`. The `Database.PostgreSQL.PQTypes.Internal.Oid` module
  provides constants for built-in types.
* Change the type of `toSQL` to `t -> Maybe Encoding`. An `Encoding` is an
  encoded value in the binary wire format. `Nothing` represents NULL. The
  `PQDest` type family and `ParamAllocator` are gone.
* Change the type of `fromSQL` to `RowDecoder t`, a monadic parser that
  consumes consecutive fields of a query result. You build a decoder of a
  compound result from the decoders of its fields. The `PQBase` type family
  is gone.
* Remove the `PQFormat` superclass of `FromSQL`.
* Remove the `FromRow` class. The row fetching functions (`foldrDB`,
  `foldlDB`, `mapDB_`, `fetchMany`, `fetchMaybe`, `fetchOne`) now take an
  explicit `RowDecoder` of a row, e.g. `fetchMany ((,) <$> fromSQL <*> fromSQL)`.
* Row fetching functions no longer compare the width of the query result
  with the decoder up front. The number of fields that a `RowDecoder`
  consumes is not known statically. When it decodes a row, a decoder that
  doesn't match the shape of the result throws `RowLengthMismatch`. In
  particular, a fetch from a result with no rows always succeeds.
* Reduce the maximum arity of tuples with a `ToRow` instance from 50 to 10.
  To pass a larger set of query parameters, combine rows with the `:++:`
  type. It was previously known as `:*:`. The new name avoids a conflict
  with `GHC.Generics`.
* Remove the parameter of `QueryResult` along with its `Functor` and
  `Foldable` instances. The row fetching functions that take a `RowDecoder`
  supersede them.
* Remove support for encoding of composite types along with the `Composite`,
  `CompositeRow`, `CompositeFromSQL`, `CompositeToSQL`, `CompositeArray1` and
  `CompositeArray2` machinery. The `decodeComposite` combinator decodes
  composite types. They no longer need registration, and the `csComposites`
  field of `ConnectionSettings` is gone. Decoding of anonymous composite
  types is now supported.
* Add `genericDecoder`, which decodes consecutive fields of a row into a
  product type with a `Generic` instance, e.g. a whole row into a tuple.
  Combined with `decodeComposite`, it gives generic `FromSQL` instances for
  product types that correspond to composite types.
* Add `PQFormat`, `ToSQL` and `FromSQL` instances for lists and `Vector`s
  that encode and decode PostgreSQL arrays. An array of any type with the
  relevant instances can be a query parameter, and plain `fromSQL` fetches
  it. A multi-dimensional array is a nested list or `Vector`, and mixed
  nesting of lists and `Vector`s works too. `String` still corresponds to
  `text` and `[String]` to an array of `text`. The
  `Database.PostgreSQL.PQTypes.Array` module is gone. The `fromSQLArray`
  method of `FromSQL` decodes arrays, and `fromSQLList` backs the list
  instance on top of it. `fromSQLArray` defaults to `decodeArray fromSQL`.
  An instance can override it with a faster decoder. Scalar types override
  it with the dedicated `decodeScalarArray` combinator, which decodes the
  elements with a value decoder directly.
* Remove `ArrayItemError`. An error of an array element decoder now reports
  the position of the offending element as the column of its
  `ConversionError`.
* Remove `QueryError`. When query execution produced no `PGresult`, the
  library threw it. This happened e.g. because the connection died while
  the query was sent. Such a failure now throws `LibPQError`.
* Derive the `Show` instances of `HPQTypesError` and `LibPQError`, like
  those of the other error types. They now print e.g.
  `HPQTypesError "reason"` instead of `HPQTypesError (PostgreSQL): reason`.
* Add the `Database.PostgreSQL.PQTypes.Enum` module with the `SQLEnum` and
  `SQLEnumAsText` deriving-via helpers. They map Haskell enumeration types
  to values of PostgreSQL enum types, or of other types. A scalar parameter
  works against both text and enum columns without a cast. An array is sent
  as `text[]` and needs a cast against an enum column, e.g.
  `... = ANY($1::my_enum_type[])`.
* Add `FromSQL` and `ToSQL` instances for `Scientific`, mapped to `numeric`.
  Add a `ToSQL` instance for `Word`, for symmetry with `Int`. `Int` and
  `Word` can't be decoded reliably, because their size depends on the
  architecture. Their `FromSQL` instances use `TypeError` and point at
  `Int64` and `Word64`.
* Add `FromSQL` and `ToSQL` instances for `IP` and `IPRange` from the
  `iproute` package, mapped to the `inet` and `cidr` types respectively. An
  `inet` value carries the length of its netmask alongside the address. If
  the netmask covers the whole address, the value decodes to a bare `IP`.
  Otherwise decoding fails instead of dropping the netmask.
* Add support for range types with `FromSQL` and `ToSQL` instances for
  `Range` of `Int32`, `Int64`, `Scientific`, `Day`, `LocalTime` and `UTCTime`.
  The SQL types are `int4range`, `int8range`, `numrange`, `daterange`,
  `tsrange` and `tstzrange` respectively. The
  `Database.PostgreSQL.PQTypes.Range` module defines the `Range` and `Bound`
  types, and `Database.PostgreSQL.PQTypes` re-exports them.
* Change the representation of `Interval` to mirror the wire format: three
  components (microseconds, days, months) of mutually independent duration.
  The type is now opaque. The `iyears`, `imonths`, `idays`, `ihours`,
  `iminutes`, `iseconds` and `imicroseconds` functions construct values, and
  the `Monoid` instance combines them. The sub-day functions now take
  `Int64`. The `Eq` and `Ord` instances compare the same way as the
  comparison operators of the server, i.e. by an estimate with a month
  converted at 30 days and a day at 24 hours. The `Show` instance shows the
  components of the wire format instead of a pretty-printed value.
* Reject the `infinity` and `-infinity` values of `date`, `timestamp`,
  `timestamptz` and `interval` at decode time. Previously they decoded as a
  date far outside the range that the server accepts. The encoders of `Day`,
  `LocalTime` and `UTCTime` now reject a value that doesn't fit the wire
  format instead of truncating it. Truncation silently sent a different
  date, `infinity` among them.
* When built against `libpq` >= 17, cancellation of a query that an
  exception interrupted uses the new cancellation API. If the main
  connection is encrypted, the cancellation request travels over an
  encrypted connection too.
* Fix unreliable cancellation of a query that an asynchronous exception
  interrupted. The server discards a cancellation request that reaches it
  before the backend started to execute the query. It doesn't report the
  discard in any way. An interrupted thread then waited for the query to run
  to completion. The library now repeats the request until the query ends.

# hpqtypes-1.15.0.0 (????-??-??)
* Fix a use-after-free of the buffer that holds the connection string in
  `connect`. If an asynchronous exception interrupted `connect`, the
  use-after-free was possible.
* Execution of a `COPY` statement now throws an error that says the statement
  is not supported. Previously the library silently reported success and left
  the connection in copy mode until the next query.
* Fix transaction restart handling. A restarted transaction no longer runs
  with asynchronous exceptions masked. An asynchronous exception, e.g. a
  timeout, no longer triggers a restart. A restart predicate that matches
  it makes no difference. If a transaction fails, a subsequent failure of
  its cleanup no longer masks the original exception. Previously this hid
  the exception from the restart predicate.
* Fix a bug in the on demand connection acquisition mode. If a query left
  the connection in a state that admits no further queries, e.g. because the
  connection died, the `ROLLBACK` that ended the automatic transaction of the
  query failed. Its failure masked the exception thrown from the query.
* Fix a bug in `withSavepoint`. If the action threw and the cleanup of the
  savepoint failed as well, e.g. because the connection died, the failure of
  the cleanup masked the exception of the action.
* Fix a bug in `unsafeWithoutTransaction`. If the action threw and the `BEGIN`
  that restores the transaction failed as well, e.g. because the connection
  died, the failure of the `BEGIN` masked the exception of the action.
* Row fetching functions no longer decode all rows of the result up front
  and retain them until the fold completes. They decode each row right
  before the fold function consumes it. As a result, e.g. `mapDB_` over a
  large result runs in constant additional memory.
* Add `FromSQL` and `ToSQL` instances for `Word16`, `Word32` and `Word64`.
* Add `FromSQL` and `ToSQL` instances for `Integer`, mapped to `numeric`.
* `JSON` and `JSONB` now serialize and deserialize the wrapped type with its
  `ToJSON` and `FromJSON` instances. They work for any such type and support
  `deriving via`, e.g. `deriving (PQFormat, ToSQL, FromSQL) via JSONB Config`.
  The `aesonFromSQL` and `aesonToSQL` helpers are gone. The instances for
  unparsed JSON text moved to the dedicated `RawJSON` and `RawJSONB` types,
  which wrap a strict `ByteString`. The instances for lazy `ByteString` are
  gone. The `encodeRawJSON` and `encodeRawJSONB` functions build these types
  from any type with a `ToJSON` instance. The `decodeRawJSON` and
  `decodeRawJSONB` functions convert back and return `Nothing` on a failure.
  The `eitherDecodeRawJSON` and `eitherDecodeRawJSONB` functions return the
  reason for the failure instead.
* Fix a bug in `changeAcquisitionModeTo`. If the commit of a transaction
  failed during the transition from the `AcquireAndHold` to the
  `AcquireOnDemand` mode, the library held on to an invalid connection
  object.
* Fix a bug in `withCursor`. If the enclosing transaction was in the aborted
  state, the failure of the cursor cleanup masked an exception thrown from
  the continuation. In particular, this prevented restarts of transactions
  run with a `RestartPredicate`.
* Fix a bug in `commit`, `rollback` and `unsafeWithoutTransaction`. If the
  issued `COMMIT` failed, e.g. because of a deferred constraint violation,
  the session stayed in the autocommit mode instead of starting a new
  transaction.
* Fix a bug in connection finalization. If an asynchronous exception
  interrupted the finalization while another thread used the connection,
  the other thread deadlocked permanently.
* Fix a bug in `withCursor` where an asynchronous exception cancelled the
  `CLOSE` query and left the cursor open.
* Fix a bug in `withSavepoint` where an asynchronous exception cancelled the
  `ROLLBACK TO SAVEPOINT` or `RELEASE SAVEPOINT` query. The savepoint stayed in
  place, or the transaction stayed in the aborted state.

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
