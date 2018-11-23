# hpqtypes-1.6.1.0 (2018-11-23)
* Add support for cursors.
* Don't explicitly derive Typeable for data types.
* Add support for GHC 8.6.*.

# hpqtypes-1.6.0.0 (2018-07-11)
* Convert the PQFormat class to use TypeApplications instead of
  an 'undefined :: t' argument.
* Drop support for GHC < 8.

# hpqtypes-1.5.3.0 (2018-06-04)
* add INLINE/INLINEABLE pragmas for call site specialization
* remove -O2 -funbox-strict-fields from ghc-options
* make query execution interruptible with asynchronous exceptions
* make connect interruptible with asynchronous exceptions

# hpqtypes-1.5.2.0 (2018-03-18)
* support GHC 8.4.1

# hpqtypes-1.5.1.1 (2016-09-22)
* fix test suite compilation with GHC 8
* fix lower bound of base version
* fix compilation with 'cabal new-build' and Cabal < 1.24

# hpqtypes-1.5.1 (2016-07-04)
* Do not use linux/limits.h

# hpqtypes-1.5.0 (2016-06-21)
* remove orphan MonadDB instances
* turn ConnectionSource into indexed datatype
* remove Binary wrapper and (de)serialize ByteString as bytea
* use Text instead of ByteString where appropriate
* use UTF-8 client encoding by default for compatibility with Text

# hpqtypes-1.4.5 (2016-05-30)
* fix compilation with Cabal 1.24 and GHC 8.0.1

# hpqtypes-1.4.4 (2016-01-19)
* fix lower bound of base version

# hpqtypes-1.4.3 (2015-10-09)
* remove invalid FromSQL ZonedTime instance

# hpqtypes-1.4.2 (2015-06-08)
* use strict StateT for DBT
* use catch in withTransaction only if it might be used

# hpqtypes-1.4.1 (2015-05-15)
* add support for json and jsonb sql types
* add support for lazy ByteString and Text

# hpqtypes-1.4.0 (2015-02-26)
* add support for QuickCheck 2.7
* add support for notifications
* remove SpaceMonoid, use Monoid and IsString instead
* use data-default-class package for default values
* drop Single, use Identity functor instead
* remove someSQL from IsSQL class
* remove foldlM/foldrM from MonadDB and make QueryResult instance of Foldable instead
* add support for a type representing cartesian product of rows for more composability
* do not wrap exceptions thrown from DBT in DBException unless explicitly requested
* provide custom Show instance for Interval
* add ToSQL instance for Int

# hpqtypes-1.3.2 (2015-01-27)
* replace wrong package uploaded to hackage

# hpqtypes-1.3.1 (2015-01-26)
* add support for XML type

# hpqtypes-1.3.0 (2015-01-09)
* composite: make {from,to}Composite functions pure

# hpqtypes-1.2.5 (2015-01-04)
* add support for monad-control >= 1.0.0.1

# hpqtypes-1.2.4 (2014-12-08)
* add IsString instance for Savepoint newtype
