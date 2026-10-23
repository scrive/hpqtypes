module Database.PostgreSQL.PQTypes.Model.ForeignKey
  ( ForeignKey (..)
  , ForeignKeyAction (..)
  , fkOnColumn
  , fkOnColumns
  , fkName
  , sqlAddValidFKMaybeDowntime
  , sqlAddNotValidFK
  , sqlValidateFK
  , sqlDropFK
  ) where

import Data.Monoid.Utils
import Data.Text qualified as T
import Database.PostgreSQL.PQTypes

data ForeignKey = ForeignKey
  { fkColumns :: [RawSQL ()]
  , fkRefTable :: RawSQL ()
  , fkRefColumns :: [RawSQL ()]
  , fkOnUpdate :: ForeignKeyAction
  , fkOnDelete :: ForeignKeyAction
  , fkDeferrable :: Bool
  , fkDeferred :: Bool
  , fkValidated :: Bool
  -- ^ Set to 'False' if foreign key is created as NOT
  -- VALID and left in such state (for whatever reason).
  }
  deriving (Eq, Ord, Show)

data ForeignKeyAction
  = ForeignKeyNoAction
  | ForeignKeyRestrict
  | ForeignKeyCascade
  | ForeignKeySetNull
  | ForeignKeySetDefault
  deriving (Eq, Ord, Show)

fkOnColumn :: RawSQL () -> RawSQL () -> RawSQL () -> ForeignKey
fkOnColumn column reftable refcolumn =
  fkOnColumns [column] reftable [refcolumn]

fkOnColumns :: [RawSQL ()] -> RawSQL () -> [RawSQL ()] -> ForeignKey
fkOnColumns columns reftable refcolumns =
  ForeignKey
    { fkColumns = columns
    , fkRefTable = reftable
    , fkRefColumns = refcolumns
    , fkOnUpdate = ForeignKeyCascade
    , fkOnDelete = ForeignKeyNoAction
    , fkDeferrable = True
    , fkDeferred = False
    , fkValidated = True
    }

fkName :: RawSQL () -> ForeignKey -> RawSQL ()
fkName tname ForeignKey {..} =
  shorten $
    mconcat
      [ "fk__"
      , tname
      , "__"
      , mintercalate "__" fkColumns
      , "__"
      , fkRefTable
      ]
  where
    -- PostgreSQL's limit for identifier is 63 characters
    shorten = flip rawSQL () . T.take 63 . unRawSQL

-- | Add valid foreign key. Warning: PostgreSQL acquires SHARE ROW EXCLUSIVE
-- lock (that prevents data updates) on both modified and referenced table for
-- the duration of the creation. If this is not acceptable, use
-- 'sqlAddNotValidFK' and 'sqlValidateFK'.
sqlAddValidFKMaybeDowntime :: RawSQL () -> ForeignKey -> RawSQL ()
sqlAddValidFKMaybeDowntime = sqlAddFK_ True

-- | Add foreign key marked as NOT VALID. This avoids potentially long
-- validation blocking updates to both modified and referenced table for its
-- duration. However, keys created as such need to be validated later using
-- 'sqlValidateFK'.
sqlAddNotValidFK :: RawSQL () -> ForeignKey -> RawSQL ()
sqlAddNotValidFK = sqlAddFK_ False

-- | Validate foreign key previously created as NOT VALID.
sqlValidateFK :: RawSQL () -> ForeignKey -> RawSQL ()
sqlValidateFK tname fk = "VALIDATE CONSTRAINT" <+> fkName tname fk

sqlAddFK_ :: Bool -> RawSQL () -> ForeignKey -> RawSQL ()
sqlAddFK_ valid tname fk@ForeignKey {..} =
  mconcat
    [ "ADD CONSTRAINT" <+> fkName tname fk <+> "FOREIGN KEY ("
    , mintercalate ", " fkColumns
    , ") REFERENCES" <+> fkRefTable <+> "("
    , mintercalate ", " fkRefColumns
    , ") ON UPDATE" <+> foreignKeyActionToSQL fkOnUpdate
    , "  ON DELETE" <+> foreignKeyActionToSQL fkOnDelete
    , " " <> if fkDeferrable then "DEFERRABLE" else "NOT DEFERRABLE"
    , " INITIALLY" <+> if fkDeferred then "DEFERRED" else "IMMEDIATE"
    , if valid then "" else " NOT VALID"
    ]
  where
    foreignKeyActionToSQL ForeignKeyNoAction = "NO ACTION"
    foreignKeyActionToSQL ForeignKeyRestrict = "RESTRICT"
    foreignKeyActionToSQL ForeignKeyCascade = "CASCADE"
    foreignKeyActionToSQL ForeignKeySetNull = "SET NULL"
    foreignKeyActionToSQL ForeignKeySetDefault = "SET DEFAULT"

sqlDropFK :: RawSQL () -> ForeignKey -> RawSQL ()
sqlDropFK tname fk = "DROP CONSTRAINT" <+> fkName tname fk
