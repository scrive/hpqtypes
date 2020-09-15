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
