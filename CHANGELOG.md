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
