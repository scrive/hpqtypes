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
