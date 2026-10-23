module Database.PostgreSQL.PQTypes.Checks
  ( -- * Definitions
    DatabaseDefinitions (..)
  , emptyDbDefinitions

    -- * Checks
  , checkDatabase
  , checkDatabaseWithReport
  , createTable
  , createDomain

    -- * Options
  , ExtrasOptions (..)
  , defaultExtrasOptions
  , ObjectsValidationMode (..)

    -- * Migrations
  , migrateDatabase

    -- * Internals for tests
  , ValidationResult
  , validationError
  , validationInfo
  ) where

import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Extra (mconcatMapM)
import Control.Monad.Writer as W
import Data.Foldable (foldMap')
import Data.Function
import Data.Int
import Data.List (partition)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid.Utils
import Data.Set qualified as S
import Data.String qualified
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (cast)
import Database.PostgreSQL.PQTypes
import GHC.Stack (HasCallStack)
import Log
import TextShow

import Database.PostgreSQL.PQTypes.Checks.Util
import Database.PostgreSQL.PQTypes.ExtrasOptions
import Database.PostgreSQL.PQTypes.Migrate
import Database.PostgreSQL.PQTypes.Model
import Database.PostgreSQL.PQTypes.SQL.Builder
import Database.PostgreSQL.PQTypes.Versions

headExc :: String -> [a] -> a
headExc s [] = error s
headExc _ (x : _) = x

data DatabaseDefinitions = DatabaseDefinitions
  { dbExtensions :: [Extension]
  , dbComposites :: [CompositeType]
  , dbEnums :: [EnumType]
  , dbDomains :: [Domain]
  , dbTables :: [Table]
  }

emptyDbDefinitions :: DatabaseDefinitions
emptyDbDefinitions = DatabaseDefinitions [] [] [] [] []

----------------------------------------

-- | Run migrations and check the database structure.
migrateDatabase
  :: (MonadIO m, MonadDB m, MonadLog m, MonadMask m)
  => ExtrasOptions
  -> DatabaseDefinitions
  -> [Migration m]
  -> m ()
migrateDatabase
  options
  DatabaseDefinitions
    { dbExtensions = extensions
    , dbComposites = composites
    , dbEnums = enums
    , dbDomains = domains
    , dbTables = tables
    }
  migrations = do
    checkPostgresVersion
    setDBTimeZoneToUTC
    mapM_ checkExtension extensions
    tablesWithVersions <- getTableVersions (tableVersions : tables)
    -- 'checkDBConsistency' also performs migrations.
    checkDBConsistency options domains enums tablesWithVersions migrations
    resultCheck
      =<< checkCompositesStructure
        tablesWithVersions
        CreateCompositesIfDatabaseEmpty
        (eoObjectsValidationMode options)
        composites
    resultCheck =<< checkEnumTypes enums
    resultCheck =<< checkDomainsStructure domains
    resultCheck =<< checkDBStructure options tablesWithVersions
    resultCheck =<< checkTablesWereDropped migrations

    when (eoObjectsValidationMode options == DontAllowUnknownObjects) $ do
      resultCheck =<< checkUnknownTables tables
      resultCheck =<< checkExistenceOfVersionsForTables (tableVersions : tables)

    -- After migrations are done make sure the table versions are correct.
    resultCheck . checkVersions options =<< getTableVersions (tableVersions : tables)

    -- everything is OK, commit changes
    commit

-- | Run checks on the database structure and whether the database needs to be
-- migrated. Will do a full check of DB structure.
checkDatabaseWithReport
  :: forall m
   . (MonadDB m, MonadLog m, MonadThrow m)
  => ExtrasOptions
  -> DatabaseDefinitions
  -> m ValidationResult
checkDatabaseWithReport
  options
  DatabaseDefinitions
    { dbExtensions = _
    , dbComposites = composites
    , dbEnums = enums
    , dbDomains = domains
    , dbTables = tables
    } = execWriterT $ do
    lift checkPostgresVersion
    (_, report) <- W.listen $ do
      tablesWithVersions <- getTableVersions (tableVersions : tables)
      tell $ checkVersions options tablesWithVersions
      tell
        =<< checkCompositesStructure
          tablesWithVersions
          DontCreateComposites
          (eoObjectsValidationMode options)
          composites
      tell =<< checkEnumTypes enums
      tell =<< checkDomainsStructure domains
      tell =<< checkDBStructure options tablesWithVersions
      when (eoObjectsValidationMode options == DontAllowUnknownObjects) $ do
        tell =<< checkUnknownTables tables
        tell =<< checkExistenceOfVersionsForTables (tableVersions : tables)

    -- Check initial setups only after database structure is considered
    -- consistent as before that some of the checks may fail internally.
    unless (resultHasErrors report) $
      tell =<< lift (checkInitialSetups tables)
    where
      checkInitialSetups :: [Table] -> m ValidationResult
      checkInitialSetups = mconcatMapM checkInitialSetupForTable

      checkInitialSetupForTable table = case tblInitialSetup table of
        Nothing -> return mempty
        Just setup ->
          checkInitialSetup setup >>= \case
            True -> return mempty
            False ->
              return . validationError $
                "Initial setup for table '"
                  <> tblNameText table
                  <> "' is not valid"

-- | An equivalent to `checkDatabaseWithReport opts dbDefs >>= resultCheck`.
checkDatabase
  :: forall m
   . (MonadDB m, MonadLog m, MonadThrow m)
  => ExtrasOptions
  -> DatabaseDefinitions
  -> m ()
checkDatabase options dbDefinitions =
  checkDatabaseWithReport options dbDefinitions >>= resultCheck

-- | Return SQL fragment of current catalog within quotes
currentCatalog :: (MonadDB m, MonadThrow m) => m (RawSQL ())
currentCatalog = do
  runSQL_ "SELECT current_catalog::text"
  dbname <- fetchOne runIdentity
  return $ unsafeSQL $ "\"" ++ dbname ++ "\""

-- | Check for a given extension. We need to read from 'pg_extension'
-- table as Amazon RDS limits usage of 'CREATE EXTENSION IF NOT EXISTS'.
checkExtension :: (MonadDB m, MonadLog m, MonadThrow m) => Extension -> m ()
checkExtension (Extension extension) = do
  logInfo_ $ "Checking for extension '" <> txtExtension <> "'"
  extensionExists <- runQuery01 . sqlSelect "pg_extension" $ do
    sqlResult "TRUE"
    sqlWhereEq "extname" $ unRawSQL extension
  if not extensionExists
    then do
      logInfo_ $ "Creating extension '" <> txtExtension <> "'"
      runSQL_ $ "CREATE EXTENSION IF NOT EXISTS" <+> raw extension
    else logInfo_ $ "Extension '" <> txtExtension <> "' exists"
  where
    txtExtension = unRawSQL extension

-- | Check whether the database returns timestamps in UTC, and set the
-- timezone to UTC if it doesn't.
setDBTimeZoneToUTC :: (MonadDB m, MonadLog m, MonadThrow m) => m ()
setDBTimeZoneToUTC = do
  runSQL_ "SHOW timezone"
  timezone :: String <- fetchOne runIdentity
  when (timezone /= "UTC") $ do
    dbname <- currentCatalog
    logInfo_ $
      "Setting '"
        <> unRawSQL dbname
        <> "' database to return timestamps in UTC"
    runQuery_ $ "ALTER DATABASE" <+> dbname <+> "SET TIMEZONE = 'UTC'"
    -- Setting the database timezone doesn't change the session timezone.
    runSQL_ "SET timezone = 'UTC'"

-- | Get the names of all user-defined tables that actually exist in
-- the DB.
getDBTableNames :: MonadDB m => m [Text]
getDBTableNames = do
  runQuery_ $ sqlSelect "information_schema.tables" $ do
    sqlResult "table_name::text"
    sqlWhere "table_name <> 'table_versions'"
    sqlWhere "table_type = 'BASE TABLE'"
    sqlWhereExists $ sqlSelect "unnest(current_schemas(false)) as cs" $ do
      sqlResult "TRUE"
      sqlWhere "cs = table_schema"
  fetchMany runIdentity

checkVersions :: ExtrasOptions -> TablesWithVersions -> ValidationResult
checkVersions options = mconcat . map checkVersion
  where
    checkVersion :: (Table, Int32) -> ValidationResult
    checkVersion (t@Table {..}, v)
      | if eoAllowHigherTableVersions options
          then tblVersion <= v
          else tblVersion == v =
          mempty
      | v == 0 =
          validationError $
            "Table '" <> tblNameText t <> "' must be created"
      | otherwise =
          validationError $
            "Table '"
              <> tblNameText t
              <> "' must be migrated"
              <+> showt v
              <+> "->"
              <+> showt tblVersion

-- | Check that there's a 1-to-1 correspondence between the list of
-- 'Table's and what's actually in the database.
checkUnknownTables :: (MonadDB m, MonadLog m) => [Table] -> m ValidationResult
checkUnknownTables tables = do
  dbTableNames <- getDBTableNames
  let tableNames = map (unRawSQL . tblName) tables
      absent = dbTableNames L.\\ tableNames
      notPresent = tableNames L.\\ dbTableNames

  if (not . null $ absent) || (not . null $ notPresent)
    then do
      mapM_ (logInfo_ . (<+>) "Unknown table:") absent
      mapM_ (logInfo_ . (<+>) "Table not present in the database:") notPresent
      return $
        validateIsNull "Unknown tables:" absent
          <> validateIsNull "Tables not present in the database:" notPresent
    else return mempty

validateIsNull :: Text -> [Text] -> ValidationResult
validateIsNull _ [] = mempty
validateIsNull msg ts = validationError $ msg <+> T.intercalate ", " ts

-- | Check that there's a 1-to-1 correspondence between the list of
-- 'Table's and what's actually in the table 'table_versions'.
checkExistenceOfVersionsForTables
  :: (MonadDB m, MonadLog m)
  => [Table]
  -> m ValidationResult
checkExistenceOfVersionsForTables tables = do
  runQuery_ $ sqlSelect "table_versions" $ do
    sqlResult "name::text"
  (existingTableNames :: [Text]) <- fetchMany runIdentity

  let tableNames = map (unRawSQL . tblName) tables
      absent = existingTableNames L.\\ tableNames
      notPresent = tableNames L.\\ existingTableNames

  if (not . null $ absent) || (not . null $ notPresent)
    then do
      mapM_ (logInfo_ . (<+>) "Unknown entry in 'table_versions':") absent
      mapM_
        (logInfo_ . (<+>) "Table not present in the 'table_versions':")
        notPresent
      return $
        validateIsNull "Unknown entry in table_versions':" absent
          <> validateIsNull "Tables not present in the 'table_versions':" notPresent
    else return mempty

checkDomainsStructure
  :: (MonadDB m, MonadThrow m)
  => [Domain]
  -> m ValidationResult
checkDomainsStructure defs = fmap mconcat . forM defs $ \def -> do
  runQuery_ . sqlSelect "pg_catalog.pg_type t1" $ do
    sqlResult "t1.typname::text" -- name
    sqlResult
      "(SELECT pg_catalog.format_type(t2.oid, t2.typtypmod) \
      \FROM pg_catalog.pg_type t2 \
      \WHERE t2.oid = t1.typbasetype)" -- type
    sqlResult "NOT t1.typnotnull" -- nullable
    sqlResult "t1.typdefault" -- default value
    sqlResult
      "ARRAY(SELECT c.conname::text FROM pg_catalog.pg_constraint c \
      \WHERE c.contype = 'c' AND c.contypid = t1.oid \
      \ORDER by c.oid)" -- constraint names
    sqlResult
      "ARRAY(SELECT regexp_replace(pg_get_constraintdef(c.oid, true), '\
      \CHECK \\((.*)\\)', '\\1') FROM pg_catalog.pg_constraint c \
      \WHERE c.contype = 'c' AND c.contypid = t1.oid \
      \ORDER by c.oid)" -- constraint definitions
    sqlResult
      "ARRAY(SELECT c.convalidated FROM pg_catalog.pg_constraint c \
      \WHERE c.contype = 'c' AND c.contypid = t1.oid \
      \ORDER by c.oid)" -- are constraints validated?
    sqlWhereEq "t1.typname" $ unRawSQL $ domName def
  mdom <- fetchMaybe $
    \(dname, dtype, nullable, defval, cnames, conds, valids) ->
      Domain
        { domName = unsafeSQL dname
        , domType = dtype
        , domNullable = nullable
        , domDefault = unsafeSQL <$> defval
        , domChecks =
            mkChecks $
              zipWith3
                ( \cname cond validated ->
                    Check
                      { chkName = unsafeSQL cname
                      , chkCondition = unsafeSQL cond
                      , chkValidated = validated
                      }
                )
                (unArray1 cnames)
                (unArray1 conds)
                (unArray1 valids)
        }
  return $ case mdom of
    Just dom
      | dom /= def ->
          topMessage "domain" (unRawSQL $ domName dom) $
            mconcat
              [ compareAttr dom def "name" domName
              , compareAttr dom def "type" domType
              , compareAttr dom def "nullable" domNullable
              , compareAttr dom def "default" domDefault
              , compareAttr dom def "checks" domChecks
              ]
      | otherwise -> mempty
    Nothing ->
      validationError $
        "Domain '"
          <> unRawSQL (domName def)
          <> "' doesn't exist in the database"
  where
    compareAttr
      :: (Eq a, Show a)
      => Domain
      -> Domain
      -> Text
      -> (Domain -> a)
      -> ValidationResult
    compareAttr dom def attrname attr
      | attr dom == attr def = mempty
      | otherwise =
          validationError $
            "Attribute '"
              <> attrname
              <> "' does not match (database:"
              <+> T.pack (show $ attr dom)
              <> ", definition:"
              <+> T.pack (show $ attr def)
              <> ")"

checkEnumTypes
  :: (MonadDB m, MonadThrow m)
  => [EnumType]
  -> m ValidationResult
checkEnumTypes defs = fmap mconcat . forM defs $ \defEnum -> do
  runQuery_ . sqlSelect "pg_catalog.pg_type t" $ do
    sqlResult "t.typname::text" -- name
    sqlResult
      "ARRAY(SELECT e.enumlabel::text FROM pg_catalog.pg_enum e WHERE e.enumtypid = t.oid ORDER BY e.enumsortorder)" -- values
    sqlWhereEq "t.typname" $ unRawSQL $ etName defEnum
  enum <- fetchMaybe $
    \(enumName, enumValues) ->
      EnumType
        { etName = unsafeSQL enumName
        , etValues = map unsafeSQL $ unArray1 enumValues
        }
  pure $ case enum of
    Just dbEnum -> do
      let enumName = unRawSQL $ etName defEnum
          dbValues = map unRawSQL $ etValues dbEnum
          defValues = map unRawSQL $ etValues defEnum
          dbSet = S.fromList dbValues
          defSet = S.fromList defValues
      if
        | dbValues == defValues -> mempty
        | L.sort dbValues == L.sort defValues ->
            validationInfo $
              "Enum '"
                <> enumName
                <> "' has same values, but differs in order (database: "
                <> T.pack (show dbValues)
                <> ", definition: "
                <> T.pack (show defValues)
                <> "). "
                <> "This isn't usually a problem, unless the enum is used for ordering."
        | S.isSubsetOf defSet dbSet ->
            validationInfo $
              "Enum '"
                <> enumName
                <> "' has all necessary values, but the database has additional ones "
                <> "(database: "
                <> T.pack (show dbValues)
                <> ", definition: "
                <> T.pack (show defValues)
                <> ")"
        | otherwise ->
            validationError $
              "Enum '"
                <> enumName
                <> "' does not match (database: "
                <> T.pack (show dbValues)
                <> ", definition: "
                <> T.pack (show defValues)
                <> ")"
    Nothing ->
      validationError $
        "Enum '"
          <> unRawSQL (etName defEnum)
          <> "' doesn't exist in the database"

-- | Check that the tables that must have been dropped are actually
-- missing from the DB.
checkTablesWereDropped
  :: (MonadDB m, MonadThrow m)
  => [Migration m]
  -> m ValidationResult
checkTablesWereDropped mgrs = do
  let droppedTableNames =
        [ mgrTableName mgr
        | mgr <- mgrs
        , isDropTableMigration mgr
        ]
  fmap mconcat . forM droppedTableNames $
    \tblName -> do
      mver <- checkTableVersion (T.unpack . unRawSQL $ tblName)
      return $
        if isNothing mver
          then mempty
          else
            validationError $
              "The table '"
                <> unRawSQL tblName
                <> "' that must have been dropped"
                <> " is still present in the database."

data CompositesCreationMode
  = CreateCompositesIfDatabaseEmpty
  | DontCreateComposites
  deriving (Eq)

-- | Check that there is 1 to 1 correspondence between composite types in the
-- database and the list of their code definitions.
checkCompositesStructure
  :: MonadDB m
  => TablesWithVersions
  -> CompositesCreationMode
  -> ObjectsValidationMode
  -> [CompositeType]
  -> m ValidationResult
checkCompositesStructure tablesWithVersions ccm ovm compositeList =
  getDBCompositeTypes >>= \case
    [] | noTablesPresent tablesWithVersions && ccm == CreateCompositesIfDatabaseEmpty -> do
      -- DB is not initialized, create composites if there are any defined.
      mapM_ (runQuery_ . sqlCreateComposite) compositeList
      return mempty
    dbCompositeTypes ->
      pure $
        mconcat
          [ checkNotPresentComposites
          , checkDatabaseComposites
          ]
      where
        compositeMap =
          M.fromList $
            map ((unRawSQL . ctName) &&& ctColumns) compositeList

        checkNotPresentComposites =
          let notPresent =
                S.toList $
                  M.keysSet compositeMap
                    S.\\ S.fromList (map (unRawSQL . ctName) dbCompositeTypes)
          in validateIsNull "Composite types not present in the database:" notPresent

        checkDatabaseComposites = mconcat . (`map` dbCompositeTypes) $ \dbComposite ->
          let cname = unRawSQL $ ctName dbComposite
          in case cname `M.lookup` compositeMap of
               Just columns ->
                 topMessage "composite type" cname $
                   checkColumns 1 columns (ctColumns dbComposite)
               Nothing -> case ovm of
                 AllowUnknownObjects -> mempty
                 DontAllowUnknownObjects ->
                   validationError $
                     mconcat
                       [ "Composite type '"
                       , T.pack $ show dbComposite
                       , "' from the database doesn't have a corresponding code definition"
                       ]
          where
            checkColumns
              :: Int -> [CompositeColumn] -> [CompositeColumn] -> ValidationResult
            checkColumns _ [] [] = mempty
            checkColumns _ rest [] =
              validationError $
                objectHasLess "Composite type" "columns" rest
            checkColumns _ [] rest =
              validationError $
                objectHasMore "Composite type" "columns" rest
            checkColumns !n (d : defs) (c : cols) =
              mconcat
                [ validateNames $ ccName d == ccName c
                , validateTypes $ ccType d == ccType c
                , checkColumns (n + 1) defs cols
                ]
              where
                validateNames True = mempty
                validateNames False =
                  validationError $
                    errorMsg ("no. " <> showt n) "names" (unRawSQL . ccName)

                validateTypes True = mempty
                validateTypes False =
                  validationError $
                    errorMsg (unRawSQL $ ccName d) "types" (T.pack . show . ccType)

                errorMsg ident attr f =
                  "Column '"
                    <> ident
                    <> "' differs in"
                    <+> attr
                    <+> "(database:"
                    <+> f c
                    <> ", definition:"
                    <+> f d
                    <> ")."

-- | Checks whether the database is consistent.
checkDBStructure
  :: forall m
   . (MonadDB m, MonadThrow m)
  => ExtrasOptions
  -> TablesWithVersions
  -> m ValidationResult
checkDBStructure options tables = fmap mconcat . forM tables $ \(table, version) -> do
  result <- topMessage "table" (tblNameText table) <$> checkTableStructure table
  -- If we allow higher table versions in the database, show inconsistencies as
  -- info messages only.
  return $
    if eoAllowHigherTableVersions options && tblVersion table < version
      then validationErrorsToInfos result
      else result
  where
    checkTableStructure :: Table -> m ValidationResult
    checkTableStructure table@Table {..} = do
      -- get table description from pg_catalog as describeTable
      -- mechanism from HDBC doesn't give accurate results
      runQuery_ $ sqlSelect "pg_catalog.pg_attribute a" $ do
        sqlResult "a.attname::text"
        sqlResult "pg_catalog.format_type(a.atttypid, a.atttypmod)"
        sqlResult . parenthesize . toSQLCommand $
          sqlSelect "pg_catalog.pg_collation c, pg_catalog.pg_type t" $ do
            sqlResult "c.collname::text"
            -- `typcollation` specifies the default collation of the type (if
            -- any), and `attcollation` is the collation of the column.
            sqlWhere "c.oid = a.attcollation AND t.oid = a.atttypid AND a.attcollation <> t.typcollation"
        sqlResult "NOT a.attnotnull"
        sqlResult . parenthesize . toSQLCommand $
          sqlSelect "pg_catalog.pg_attrdef d" $ do
            sqlResult "pg_catalog.pg_get_expr(d.adbin, d.adrelid)"
            sqlWhere "d.adrelid = a.attrelid"
            sqlWhere "d.adnum = a.attnum"
            sqlWhere "a.atthasdef"
        sqlWhere "a.attnum > 0"
        sqlWhere "NOT a.attisdropped"
        sqlWhereEqSql "a.attrelid" $ sqlGetTableID tblName
        sqlOrderBy "a.attnum"
      desc <- fetchMany fetchTableColumn

      -- get info about constraints from pg_catalog
      pk <- sqlGetPrimaryKey tblName
      runQuery_ $ sqlGetChecks tblName
      checks <- fetchMany fetchTableCheck
      runQuery_ $ sqlGetIndexes tblName Nothing
      indexes <- fetchMany fetchTableIndex
      runQuery_ $ sqlGetForeignKeys tblName
      fkeys <- fetchMany fetchForeignKey
      triggers <- getDBTriggers tblName
      checkedOverlaps <- checkOverlappingIndexes tblName
      return $
        mconcat
          [ checkColumns 1 tblColumns desc
          , checkPrimaryKey tblPrimaryKey pk
          , checkChecks tblChecks checks
          , checkIndexes tblIndexes indexes
          , checkForeignKeys tblForeignKeys fkeys
          , checkForeignKeyIndexes tblPrimaryKey tblForeignKeys tblIndexes
          , checkTriggers tblTriggers triggers
          , checkedOverlaps
          ]
      where
        fetchTableColumn
          :: (String, ColumnType, Maybe Text, Bool, Maybe String) -> TableColumn
        fetchTableColumn (name, ctype, collation, nullable, mdefault) =
          TableColumn
            { colName = unsafeSQL name
            , colType = ctype
            , colCollation = flip rawSQL () <$> collation
            , colNullable = nullable
            , colDefault = unsafeSQL <$> mdefault
            }

        checkColumns
          :: Int -> [TableColumn] -> [TableColumn] -> ValidationResult
        checkColumns _ [] [] = mempty
        checkColumns _ rest [] =
          validationError $
            objectHasLess "Table" "columns" rest
        checkColumns _ [] rest =
          validationError $
            objectHasMore "Table" "columns" rest
        checkColumns !n (d : defs) (c : cols) =
          mconcat
            [ validateNames $ colName d == colName c
            , -- bigserial == bigint + autoincrement and there is no
              -- distinction between them after table is created.
              validateTypes $
                colType d == colType c
                  || (colType d == BigSerialT && colType c == BigIntT)
            , -- There is a problem with default values determined by
              -- sequences as they're implicitly specified by db, so
              -- let's omit them in such case.
              validateDefaults $
                colDefault d == colDefault c
                  || ( isNothing (colDefault d)
                         && (T.isPrefixOf "nextval('" . unRawSQL <$> colDefault c)
                           == Just True
                     )
            , validateNullables $ colNullable d == colNullable c
            , checkColumns (n + 1) defs cols
            ]
          where
            validateNames True = mempty
            validateNames False =
              validationError $
                errorMsg ("no. " <> showt n) "names" (unRawSQL . colName)

            validateTypes True = mempty
            validateTypes False =
              validationError $
                errorMsg cname "types" (T.pack . show . colType)
                  <+> sqlHint ("TYPE" <+> columnTypeToSQL (colType d))

            validateNullables True = mempty
            validateNullables False =
              validationError $
                errorMsg cname "nullables" (showt . colNullable)
                  <+> sqlHint
                    ( (if colNullable d then "DROP" else "SET")
                        <+> "NOT NULL"
                    )

            validateDefaults True = mempty
            validateDefaults False =
              validationError $
                errorMsg cname "defaults" (showt . fmap unRawSQL . colDefault)
                  <+> sqlHint set_default
              where
                set_default = case colDefault d of
                  Just v -> "SET DEFAULT" <+> v
                  Nothing -> "DROP DEFAULT"

            cname = unRawSQL $ colName d
            errorMsg ident attr f =
              "Column '"
                <> ident
                <> "' differs in"
                <+> attr
                <+> "(table:"
                <+> f c
                <> ", definition:"
                <+> f d
                <> ")."
            sqlHint sql =
              "(HINT: SQL for making the change is: ALTER TABLE"
                <+> tblNameText table
                <+> "ALTER COLUMN"
                <+> unRawSQL (colName d)
                <+> unRawSQL sql
                <> ")"

        checkPrimaryKey
          :: Maybe PrimaryKey
          -> Maybe (PrimaryKey, RawSQL ())
          -> ValidationResult
        checkPrimaryKey mdef mpk =
          mconcat
            [ checkEquality "PRIMARY KEY" def (map fst pk)
            , checkNames (const (pkName tblName)) pk
            , if eoEnforcePKs options
                then checkPKPresence tblName mdef mpk
                else mempty
            ]
          where
            def = maybeToList mdef
            pk = maybeToList mpk

        checkChecks :: [Check] -> [Check] -> ValidationResult
        checkChecks defs checks =
          mapValidationResult id mapErrs (checkEquality "CHECKs" defs checks)
          where
            mapErrs [] = []
            mapErrs errmsgs =
              errmsgs
                <> [ " (HINT: If checks are equal modulo number of \
                     \ parentheses/whitespaces used in conditions, \
                     \ just copy and paste expected output into source code)"
                   ]

        checkIndexes
          :: [TableIndex]
          -> [(TableIndex, RawSQL ())]
          -> ValidationResult
        checkIndexes defs allIndexes =
          mconcat $
            checkEquality "INDEXes" defs (map fst indexes)
              : checkNames (indexName tblName) indexes
              : map localIndexInfo localIndexes
          where
            localIndexInfo (index, name) =
              validationInfo $
                T.concat
                  [ "Found a local index '"
                  , unRawSQL name
                  , "': "
                  , T.pack (show index)
                  ]

            (localIndexes, indexes) = (`partition` allIndexes) $ \(_, name) ->
              -- Manually created indexes for ad-hoc improvements.
              "local_" `T.isPrefixOf` unRawSQL name
                -- Indexes related to the REINDEX operation, see
                -- https://www.postgresql.org/docs/15/sql-reindex.html
                || "_ccnew" `T.isSuffixOf` unRawSQL name
                || "_ccold" `T.isSuffixOf` unRawSQL name

        checkForeignKeys
          :: [ForeignKey]
          -> [(ForeignKey, RawSQL ())]
          -> ValidationResult
        checkForeignKeys defs fkeys =
          mconcat
            [ checkEquality "FOREIGN KEYs" defs (map fst fkeys)
            , checkNames (fkName tblName) fkeys
            ]

        checkForeignKeyIndexes :: Maybe PrimaryKey -> [ForeignKey] -> [TableIndex] -> ValidationResult
        checkForeignKeyIndexes pkey foreignKeys indexes =
          if eoCheckForeignKeysIndexes options
            then foldMap' go foreignKeys
            else mempty
          where
            -- Map index on the given table name to a list of list of names
            -- so that index on a and index on (b, c) becomes [[a], [b, c,]].
            allIndexes :: [[RawSQL ()]]
            allIndexes = fmap (fmap indexColumnName . idxColumns) . filter (isNothing . idxWhere) $ indexes

            allCoverage :: [[RawSQL ()]]
            allCoverage = maybe [] pkColumns pkey : allIndexes
            -- A foreign key is covered if it is a prefix of a list of indices.
            -- So a FK on a is covered by an index on (a, b) but not an index on (b, a).
            coveredFK :: ForeignKey -> [[RawSQL ()]] -> Bool
            coveredFK fk = any (\idx -> fkColumns fk `L.isPrefixOf` idx)

            go :: ForeignKey -> ValidationResult
            go fk =
              let columns = map unRawSQL (fkColumns fk)
              in if coveredFK fk allCoverage
                   then mempty
                   else validationError $ mconcat ["\n  ● Foreign key '(", T.intercalate "," columns, ")' is missing an index"]

        checkTriggers :: [Trigger] -> [Trigger] -> ValidationResult
        checkTriggers defs triggers =
          mapValidationResult id mapErrs $ checkEquality "TRIGGERs" defs triggers
          where
            mapErrs [] = []
            mapErrs errmsgs =
              errmsgs
                <> [ "(HINT: If WHEN clauses are equal modulo number of parentheses, whitespace, \
                     \case of variables or type casts used in conditions, just copy and paste \
                     \expected output into source code.)"
                   ]

        checkOverlappingIndexes :: MonadDB m => RawSQL () -> m ValidationResult
        checkOverlappingIndexes tableName =
          if eoCheckOverlappingIndexes options
            then go
            else pure mempty
          where
            go = do
              let handleOverlap (contained, contains) =
                    mconcat
                      [ "\n  ●  Index "
                      , contains
                      , " contains index "
                      , contained
                      ]
              runSQL_ $ checkOverlappingIndexesQuery tableName
              overlaps <- fetchMany handleOverlap
              pure $
                if null overlaps
                  then mempty
                  else validationError . T.unlines $ "Some indexes are overlapping" : overlaps

-- | Checks whether database is consistent, performing migrations if
-- necessary. Requires all table names to be in lower case.
--
-- The migrations list must have the following properties:
--   * consecutive 'mgrFrom' numbers
--   * no duplicates
--   * all 'mgrFrom' are less than table version number of the table in
--     the 'tables' list
checkDBConsistency
  :: forall m
   . (MonadIO m, MonadDB m, MonadLog m, MonadMask m)
  => ExtrasOptions
  -> [Domain]
  -> [EnumType]
  -> TablesWithVersions
  -> [Migration m]
  -> m ()
checkDBConsistency options domains enums tablesWithVersions migrations = do
  -- Check the validity of the migrations list.
  validateMigrations
  validateDropTableMigrations

  -- Load version numbers of the tables that actually exist in the DB.
  dbTablesWithVersions <- getDBTableVersions

  if noTablesPresent tablesWithVersions
    -- No tables are present, create everything from scratch.
    then do
      createDBSchema
      initializeDB

    -- Migration mode.
    else do
      -- Additional validity checks for the migrations list.
      validateMigrationsAgainstDB
        [ (tblName table, tblVersion table, actualVer)
        | (table, actualVer) <- tablesWithVersions
        ]
      validateDropTableMigrationsAgainstDB dbTablesWithVersions
      -- Run migrations, if necessary.
      runMigrations dbTablesWithVersions
  where
    tables = map fst tablesWithVersions

    errorInvalidMigrations :: HasCallStack => [RawSQL ()] -> a
    errorInvalidMigrations tblNames =
      error $
        "checkDBConsistency: invalid migrations for tables"
          <+> L.intercalate ", " (map (T.unpack . unRawSQL) tblNames)

    checkMigrationsListValidity :: Table -> [Int32] -> [Int32] -> m ()
    checkMigrationsListValidity
      table
      presentMigrationVersions
      expectedMigrationVersions = do
        when (presentMigrationVersions /= expectedMigrationVersions) $ do
          logAttention "Migrations are invalid" $
            object
              [ "table" .= tblNameText table
              , "migration_versions" .= presentMigrationVersions
              , "expected_migration_versions" .= expectedMigrationVersions
              ]
          errorInvalidMigrations [tblName table]

    validateMigrations :: m ()
    validateMigrations = forM_ tables $ \table -> do
      -- FIXME: https://github.com/scrive/hpqtypes-extras/issues/73
      let presentMigrationVersions =
            [ mgrFrom | Migration {..} <- migrations, mgrTableName == tblName table
            ]
          expectedMigrationVersions =
            reverse
              $ take (length presentMigrationVersions)
              $ reverse [0 .. tblVersion table - 1]
      checkMigrationsListValidity
        table
        presentMigrationVersions
        expectedMigrationVersions

    validateDropTableMigrations :: m ()
    validateDropTableMigrations = do
      let droppedTableNames =
            [ mgrTableName mgr | mgr <- migrations, isDropTableMigration mgr
            ]
          tableNames =
            [tblName tbl | tbl <- tables]

      -- Check that the intersection between the 'tables' list and
      -- dropped tables is empty.
      let intersection = L.intersect droppedTableNames tableNames
      unless (null intersection) $ do
        logAttention
          ( "The intersection between tables "
              <> "and dropped tables is not empty"
          )
          $ object
            ["intersection" .= map unRawSQL intersection]
        errorInvalidMigrations
          [ tblName tbl
          | tbl <- tables
          , tblName tbl `elem` intersection
          ]

      -- Check that if a list of migrations for a given table has a
      -- drop table migration, it is unique and is the last migration
      -- in the list.
      let migrationsByTable =
            L.groupBy
              ((==) `on` mgrTableName)
              migrations
          dropMigrationLists =
            [ mgrs | mgrs <- migrationsByTable, any isDropTableMigration mgrs
            ]
          invalidMigrationLists =
            [ mgrs
            | mgrs <- dropMigrationLists
            , (not . isDropTableMigration . last $ mgrs)
                || (length . filter isDropTableMigration $ mgrs) > 1
            ]

      unless (null invalidMigrationLists) $ do
        let tablesWithInvalidMigrationLists =
              [ mgrTableName mgr | mgrs <- invalidMigrationLists, let mgr = head mgrs
              ]
        logAttention
          ( "Migration lists for some tables contain "
              <> "either multiple drop table migrations or "
              <> "a drop table migration in non-tail position."
          )
          $ object
            [ "tables"
                .= [ unRawSQL tblName
                   | tblName <- tablesWithInvalidMigrationLists
                   ]
            ]
        errorInvalidMigrations tablesWithInvalidMigrationLists

    createDBSchema :: m ()
    createDBSchema = do
      logInfo_ "Creating domains..."
      mapM_ createDomain domains
      logInfo_ "Creating enums..."
      mapM_ (runQuery_ . sqlCreateEnum) enums
      -- Create all tables with no constraints first to allow cyclic references.
      logInfo_ "Creating tables..."
      mapM_ (createTable False) tables
      logInfo_ "Creating table constraints..."
      mapM_ createTableConstraints tables
      logInfo_ "Done."

    initializeDB :: m ()
    initializeDB = do
      logInfo_ "Running initial setup for tables..."
      forM_ tables $ \t -> case tblInitialSetup t of
        Nothing -> return ()
        Just tis -> do
          logInfo_ $ "Initializing" <+> tblNameText t <> "..."
          initialSetup tis
      logInfo_ "Done."

    -- \| Input is a list of (table name, expected version, actual
    -- version) triples.
    validateMigrationsAgainstDB :: [(RawSQL (), Int32, Int32)] -> m ()
    validateMigrationsAgainstDB tablesWithVersions_ =
      forM_ tablesWithVersions_ $ \(tableName, expectedVer, actualVer) ->
        when (expectedVer /= actualVer) $
          case [ m | m@Migration {..} <- migrations, mgrTableName == tableName
               ] of
            [] ->
              error $
                "checkDBConsistency: no migrations found for table '"
                  ++ (T.unpack . unRawSQL $ tableName)
                  ++ "', cannot migrate "
                  ++ show actualVer
                  ++ " -> "
                  ++ show expectedVer
            (m : _)
              | mgrFrom m > actualVer ->
                  error $
                    "checkDBConsistency: earliest migration for table '"
                      ++ (T.unpack . unRawSQL $ tableName)
                      ++ "' is from version "
                      ++ show (mgrFrom m)
                      ++ ", cannot migrate "
                      ++ show actualVer
                      ++ " -> "
                      ++ show expectedVer
              | otherwise -> return ()

    validateDropTableMigrationsAgainstDB :: [(Text, Int32)] -> m ()
    validateDropTableMigrationsAgainstDB dbTablesWithVersions = do
      let dbTablesToDropWithVersions =
            [ (tblName, mgrFrom mgr, fromJust mver)
            | mgr <- migrations
            , isDropTableMigration mgr
            , let tblName = mgrTableName mgr
            , let mver = lookup (unRawSQL tblName) dbTablesWithVersions
            , isJust mver
            ]
      forM_ dbTablesToDropWithVersions $ \(tblName, fromVer, ver) ->
        when (fromVer /= ver) $
          -- In case when the table we're going to drop is an old
          -- version, check that there are migrations that bring it to
          -- a new one.
          validateMigrationsAgainstDB [(tblName, fromVer, ver)]

    findMigrationsToRun :: [(Text, Int32)] -> [Migration m]
    findMigrationsToRun dbTablesWithVersions =
      let tableNamesToDrop =
            [ mgrTableName mgr | mgr <- migrations, isDropTableMigration mgr
            ]
          droppedEventually :: Migration m -> Bool
          droppedEventually mgr = mgrTableName mgr `elem` tableNamesToDrop

          lookupVer :: Migration m -> Maybe Int32
          lookupVer mgr =
            lookup
              (unRawSQL $ mgrTableName mgr)
              dbTablesWithVersions

          tableDoesNotExist = isNothing . lookupVer

          -- The idea here is that we find the first migration we need
          -- to run and then just run all migrations in order after
          -- that one.
          migrationsToRun' =
            dropWhile
              ( \mgr ->
                  case lookupVer mgr of
                    -- Table doesn't exist in the DB. If it's a create
                    -- table migration and we're not going to drop the
                    -- table afterwards, this is our starting point.
                    Nothing ->
                      not $
                        (mgrFrom mgr == 0)
                          && (not . droppedEventually $ mgr)
                    -- Table exists in the DB. Run only those migrations
                    -- that have mgrFrom >= table version in the DB.
                    Just ver -> mgrFrom mgr < ver
              )
              migrations

          -- Special case: also include migrations for tables that do
          -- not exist in the DB and ARE going to be dropped if they
          -- come as a consecutive list before the starting point that
          -- we've found.
          --
          -- Case in point: createTable t, doSomethingTo t,
          -- doSomethingTo t1, dropTable t. If our starting point is
          -- 'doSomethingTo t1', and that step depends on 't',
          -- 'doSomethingTo t1' will fail. So we include 'createTable
          -- t' and 'doSomethingTo t' as well.
          l = length migrationsToRun'
          initialMigrations = drop l $ reverse migrations
          additionalMigrations' =
            takeWhile
              (\mgr -> droppedEventually mgr && tableDoesNotExist mgr)
              initialMigrations
          -- Check that all extra migration chains we've chosen begin
          -- with 'createTable', otherwise skip adding them (to
          -- prevent raising an exception during the validation step).
          additionalMigrations =
            let ret = reverse additionalMigrations'
                grps = L.groupBy ((==) `on` mgrTableName) ret
            in if any ((/=) 0 . mgrFrom . head) grps
                 then []
                 else ret
          -- Also there's no point in adding these extra migrations if
          -- we're not running any migrations to begin with.
          migrationsToRun =
            if not . null $ migrationsToRun'
              then additionalMigrations ++ migrationsToRun'
              else []
      in migrationsToRun

    runMigration :: Migration m -> m ()
    runMigration Migration {..} = do
      case mgrAction of
        StandardMigration mgrDo -> do
          logMigration
          mgrDo
          updateTableVersion
        DropTableMigration mgrDropTableMode -> do
          logInfo_ $ arrListTable mgrTableName <> "drop table"
          runQuery_ $
            sqlDropTable
              mgrTableName
              mgrDropTableMode
          runQuery_ $ sqlDelete "table_versions" $ do
            sqlWhereEq "name" (T.unpack . unRawSQL $ mgrTableName)
        CreateIndexConcurrentlyMigration mLocalIndexName idx -> do
          logMigration
          indexSet <- case mLocalIndexName of
            Nothing -> pure False
            Just localIndexName -> do
              runQuery_ $ sqlGetIndexes mgrTableName (Just localIndexName)
              fetchMaybe fetchTableIndex >>= \case
                Nothing -> do
                  logInfo_ "Local index not found"
                  pure False
                Just (localIdx, _) -> do
                  when (localIdx /= idx) $ do
                    logInfo "Local index doesn't match the definition" $
                      object
                        [ "index_local" .= show localIdx
                        , "index_definition" .= show idx
                        ]
                    error "Indexes don't match"
                  logInfo_ "Local index found, renaming"
                  runQuery_ $ "ALTER INDEX" <+> localIndexName <+> "RENAME TO" <+> indexName mgrTableName idx
                  pure True
          -- We're in auto transaction mode (as ensured at the beginning of
          -- 'checkDBConsistency'), so we need to issue explicit SQL commit,
          -- because using 'commit' function automatically starts another
          -- transaction. We don't want that as concurrent creation of index
          -- won't run inside a transaction.
          unless indexSet . unsafeWithoutTransaction $ do
            -- If migration was run before but creation of an index failed, index
            -- will be left in the database in an inactive state, so when we
            -- rerun, we need to remove it first (see
            -- https://www.postgresql.org/docs/9.6/sql-createindex.html for more
            -- information).
            runQuery_ $ "DROP INDEX CONCURRENTLY IF EXISTS" <+> indexName mgrTableName idx
            runQuery_ (sqlCreateIndexConcurrently mgrTableName idx)
          updateTableVersion
        DropIndexConcurrentlyMigration idx -> do
          logMigration
          -- We're in auto transaction mode (as ensured at the beginning of
          -- 'checkDBConsistency'), so we need to issue explicit SQL commit,
          -- because using 'commit' function automatically starts another
          -- transaction. We don't want that as concurrent dropping of index
          -- won't run inside a transaction.
          unsafeWithoutTransaction $ do
            runQuery_ (sqlDropIndexConcurrently mgrTableName idx)
          updateTableVersion
        ModifyColumnMigration cursorSql updateSql batchSize -> do
          logMigration
          when (batchSize < 1000) $ do
            error "Batch size cannot be less than 1000"
          withCursorSQL "migration_cursor" NoScroll Hold cursorSql $ \cursor -> do
            -- Vacuum should be done approximately once every 5% of the table
            -- has been updated, or every 1000 rows as a minimum.
            --
            -- In PostgreSQL, when a record is updated, a new version of this
            -- record is created. The old one is destroyed by the "vacuum"
            -- command when no transaction needs it anymore. So there's an
            -- autovacuum daemon whose purpose is to do this cleanup, and that
            -- is sufficient most of the time. We assume that it's tuned to try
            -- to keep the "bloat" (dead records) at around 10% of the table
            -- size in the environment, and it's also tuned to not saturate the
            -- server with IO operations while doing the vacuum - vacuuming is
            -- IO intensive as there are a lot of reads and rewrites, which
            -- makes it slow and costly. So, autovacuum wouldn't be able to keep
            -- up with the aggressive batch update. Therefore we need to run
            -- vacuum ourselves, to keep things in check. The 5% limit is
            -- arbitrary, but a reasonable ballpark estimate: it more or less
            -- makes sure we keep dead records in the 10% envelope and the table
            -- doesn't grow too much during the operation.
            vacuumThreshold <- max 1000 . fromIntegral . (`div` 20) <$> getRowEstimate mgrTableName
            let cursorLoop processed = do
                  cursorFetch_ cursor (CD_Forward batchSize)
                  primaryKeys <- fetchMany id
                  unless (null primaryKeys) $ do
                    updateSql primaryKeys
                    if processed + batchSize >= vacuumThreshold
                      then do
                        bracket_
                          (runSQL_ "COMMIT")
                          (runSQL_ "BEGIN")
                          (runQuery_ $ "VACUUM" <+> mgrTableName)
                        cursorLoop 0
                      else do
                        commit
                        cursorLoop (processed + batchSize)
            cursorLoop 0
          updateTableVersion
      where
        logMigration = do
          logInfo_ $
            arrListTable mgrTableName
              <> showt mgrFrom
              <+> "->"
              <+> showt (succ mgrFrom)

        updateTableVersion = do
          runQuery_ $ sqlUpdate "table_versions" $ do
            sqlSet "version" (succ mgrFrom)
            sqlWhereEq "name" (T.unpack . unRawSQL $ mgrTableName)

        -- Get the estimated number of rows of the given table. It might not
        -- work properly if the table is present in multiple database schemas.
        -- See https://wiki.postgresql.org/wiki/Count_estimate.
        getRowEstimate :: MonadDB m => RawSQL () -> m Int32
        getRowEstimate tableName = do
          runQuery_ . sqlSelect "pg_class" $ do
            sqlResult "reltuples::integer"
            -- If tables with the same name are present in multiple schemas,
            -- casting the name to regclass resolves the ambiguity using search
            -- path priority.
            sqlWhere $ "oid =" <?> unRawSQL tableName <> "::regclass"
          fetchOne runIdentity

    runMigrations :: [(Text, Int32)] -> m ()
    runMigrations dbTablesWithVersions = do
      let migrationsToRun = findMigrationsToRun dbTablesWithVersions
      validateMigrationsToRun migrationsToRun dbTablesWithVersions
      unless (null migrationsToRun) $ do
        logInfo_ "Running migrations..."
        forM_ migrationsToRun $ \mgr -> (`fix` 1) $ \loop attempts -> do
          let restartMigration query =
                if attempts < maxAttempts
                  then do
                    logInfo "Failed to acquire a lock, backing off..." $
                      object
                        [ "query" .= query
                        , "attempts" .= attempts
                        , "backoff_seconds" .= backoffSecs
                        ]
                    liftIO . threadDelay $ backoffSecs * 1_000_000
                    loop $ attempts + 1
                  else do
                    logAttention_ "Failed to lock the table, aborting."
                    error "Lock acquisition failure"
          handleJust lockNotAvailable restartMigration $ do
            runSQL_ $ "SET LOCAL lock_timeout TO" <+> intToSQL (eoLockTimeoutSecs options * 1_000)
            runMigration mgr `onException` rollback
            logInfo_ "Committing migration changes..."
            commit
        logInfo_ "Running migrations done."
      where
        backoffSecs = eoLockFailureBackoffSecs options
        maxAttempts = eoLockAttempts options

        intToSQL :: Int -> SQL
        intToSQL = unsafeSQL . show

        lockNotAvailable :: DBException -> Maybe String
        lockNotAvailable DBException {..}
          | Just DetailedQueryError {..} <- cast dbeError
          , qeErrorCode == LockNotAvailable =
              Just $ show dbeQueryContext
          | otherwise = Nothing

    validateMigrationsToRun :: [Migration m] -> [(Text, Int32)] -> m ()
    validateMigrationsToRun migrationsToRun dbTablesWithVersions = do
      let migrationsToRunGrouped :: [[Migration m]]
          migrationsToRunGrouped =
            L.groupBy ((==) `on` mgrTableName)
              . L.sortOn mgrTableName
              $ migrationsToRun -- NB: stable sort
          loc_common =
            "Database.PostgreSQL.PQTypes.Checks."
              ++ "checkDBConsistency.validateMigrationsToRun"

          lookupDBTableVer :: [Migration m] -> Maybe Int32
          lookupDBTableVer mgrGroup =
            lookup
              ( unRawSQL . mgrTableName . headExc head_err $
                  mgrGroup
              )
              dbTablesWithVersions
            where
              head_err = loc_common ++ ".lookupDBTableVer: broken invariant"

          groupsWithWrongDBTableVersions :: [([Migration m], Int32)]
          groupsWithWrongDBTableVersions =
            [ (mgrGroup, dbTableVer)
            | mgrGroup <- migrationsToRunGrouped
            , let dbTableVer = fromMaybe 0 $ lookupDBTableVer mgrGroup
            , dbTableVer /= (mgrFrom . headExc head_err $ mgrGroup)
            ]
            where
              head_err =
                loc_common
                  ++ ".groupsWithWrongDBTableVersions: broken invariant"

          mgrGroupsNotInDB :: [[Migration m]]
          mgrGroupsNotInDB =
            [ mgrGroup
            | mgrGroup <- migrationsToRunGrouped
            , isNothing $ lookupDBTableVer mgrGroup
            ]

          groupsStartingWithDropTable :: [[Migration m]]
          groupsStartingWithDropTable =
            [ mgrGroup
            | mgrGroup <- mgrGroupsNotInDB
            , isDropTableMigration . headExc head_err $ mgrGroup
            ]
            where
              head_err =
                loc_common
                  ++ ".groupsStartingWithDropTable: broken invariant"

          groupsNotStartingWithCreateTable :: [[Migration m]]
          groupsNotStartingWithCreateTable =
            [ mgrGroup
            | mgrGroup <- mgrGroupsNotInDB
            , mgrFrom (headExc head_err mgrGroup) /= 0
            ]
            where
              head_err =
                loc_common
                  ++ ".groupsNotStartingWithCreateTable: broken invariant"

          tblNames :: [[Migration m]] -> [RawSQL ()]
          tblNames grps =
            [mgrTableName . headExc head_err $ grp | grp <- grps]
            where
              head_err = loc_common ++ ".tblNames: broken invariant"

      unless (null groupsWithWrongDBTableVersions) $ do
        let tnms = tblNames . map fst $ groupsWithWrongDBTableVersions
        logAttention
          ( "There are migration chains selected for execution "
              <> "that expect a different starting table version number "
              <> "from the one in the database. "
              <> "This likely means that the order of migrations is wrong."
          )
          $ object ["tables" .= map unRawSQL tnms]
        errorInvalidMigrations tnms

      unless (null groupsStartingWithDropTable) $ do
        let tnms = tblNames groupsStartingWithDropTable
        logAttention "There are drop table migrations for non-existing tables." $
          object ["tables" .= map unRawSQL tnms]
        errorInvalidMigrations tnms

      -- NB: the following check can break if we allow renaming tables.
      unless (null groupsNotStartingWithCreateTable) $ do
        let tnms = tblNames groupsNotStartingWithCreateTable
        logAttention
          ( "Some tables haven't been created yet, but"
              <> "their migration lists don't start with a create table migration."
          )
          $ object ["tables" .= map unRawSQL tnms]
        errorInvalidMigrations tnms

-- | Type synonym for a list of tables along with their database versions.
type TablesWithVersions = [(Table, Int32)]

-- | Fail if the PostgreSQL server is older than the oldest supported version.
checkPostgresVersion :: (MonadDB m, MonadLog m, MonadThrow m) => m ()
checkPostgresVersion = do
  runSQL_ "SELECT current_setting('server_version_num')::int4, current_setting('server_version')"
  (versionNum, version) <- fetchOne $ id @(Int32, Text)
  when (versionNum < minimumVersion) . resultCheck . validationError $
    T.concat
      [ "PostgreSQL "
      , version
      , " is not supported, the minimum supported version is "
      , showt $ minimumVersion `div` 10_000
      ]
  where
    minimumVersion :: Int32
    minimumVersion = 150_000

-- | Associate each table in the list with its version as it exists in
-- the DB, or 0 if it's missing from the DB.
getTableVersions :: (MonadDB m, MonadThrow m) => [Table] -> m TablesWithVersions
getTableVersions tbls =
  sequence
    [ (\mver -> (tbl, fromMaybe 0 mver)) <$> checkTableVersion (tblNameString tbl)
    | tbl <- tbls
    ]

-- | Given a result of 'getTableVersions' check if no tables are present in the
-- database.
noTablesPresent :: TablesWithVersions -> Bool
noTablesPresent = all ((==) 0 . snd)

-- | Like 'getTableVersions', but for all user-defined tables that
-- actually exist in the DB.
getDBTableVersions :: (MonadDB m, MonadThrow m) => m [(Text, Int32)]
getDBTableVersions = do
  dbTableNames <- getDBTableNames
  sequence
    [ (\mver -> (name, fromMaybe 0 mver)) <$> checkTableVersion (T.unpack name)
    | name <- dbTableNames
    ]

-- | Check whether the table exists in the DB, and return 'Just' its
-- version if it does, or 'Nothing' if it doesn't.
checkTableVersion :: (MonadDB m, MonadThrow m) => String -> m (Maybe Int32)
checkTableVersion tblName = do
  doesExist <- runQuery01 . sqlSelect "pg_catalog.pg_class c" $ do
    sqlResult "TRUE"
    sqlLeftJoinOn "pg_catalog.pg_namespace n" "n.oid = c.relnamespace"
    sqlWhereEq "c.relname" tblName
    sqlWhere "pg_catalog.pg_table_is_visible(c.oid)"
  if doesExist
    then do
      runQuery_ $
        "SELECT version FROM table_versions WHERE name ="
          <?> tblName
      mver <- fetchMaybe runIdentity
      case mver of
        Just ver -> return $ Just ver
        Nothing ->
          error $
            "checkTableVersion: table '"
              ++ tblName
              ++ "' is present in the database, "
              ++ "but there is no corresponding version info in 'table_versions'."
    else do
      return Nothing

-- *** TABLE STRUCTURE ***

sqlGetTableID :: RawSQL () -> SQL
sqlGetTableID tableName = parenthesize . toSQLCommand $
  sqlSelect "pg_catalog.pg_class c" $ do
    sqlResult "c.oid"
    sqlLeftJoinOn "pg_catalog.pg_namespace n" "n.oid = c.relnamespace"
    sqlWhereEq "c.relname" $ unRawSQL tableName
    sqlWhere "pg_catalog.pg_table_is_visible(c.oid)"

-- *** PRIMARY KEY ***

sqlGetPrimaryKey
  :: (MonadDB m, MonadThrow m)
  => RawSQL ()
  -> m (Maybe (PrimaryKey, RawSQL ()))
sqlGetPrimaryKey tableName = do
  (mColumnNumbers :: Maybe [Int16]) <- do
    runQuery_ . sqlSelect "pg_catalog.pg_constraint" $ do
      sqlResult "conkey"
      sqlWhereEqSql "conrelid" $ sqlGetTableID tableName
      sqlWhereEq "contype" 'p'
    fetchMaybe $ unArray1 . runIdentity

  case mColumnNumbers of
    Nothing -> do return Nothing
    Just columnNumbers -> do
      columnNames <- do
        forM columnNumbers $ \k -> do
          runQuery_ . sqlSelect "pk_columns" $ do
            sqlWith "key_series" . sqlSelect "pg_constraint as c2" $ do
              sqlResult "unnest(c2.conkey) as k"
              sqlWhereEqSql "c2.conrelid" $ sqlGetTableID tableName
              sqlWhereEq "c2.contype" 'p'

            sqlWith "pk_columns" . sqlSelect "key_series" $ do
              sqlJoinOn "pg_catalog.pg_attribute as a" "a.attnum = key_series.k"
              sqlResult "a.attname::text as column_name"
              sqlResult "key_series.k as column_order"
              sqlWhereEqSql "a.attrelid" $ sqlGetTableID tableName

            sqlResult "pk_columns.column_name"
            sqlWhereEq "pk_columns.column_order" k

          fetchOne (\(Identity t) -> t :: String)

      runQuery_ . sqlSelect "pg_catalog.pg_constraint as c" $ do
        sqlWhereEq "c.contype" 'p'
        sqlWhereEqSql "c.conrelid" $ sqlGetTableID tableName
        sqlResult "c.conname::text"
        sqlResult $
          Data.String.fromString
            ("array['" <> mintercalate "', '" columnNames <> "']::text[]")

      join <$> fetchMaybe fetchPrimaryKey

fetchPrimaryKey :: (Text, Array1 Text) -> Maybe (PrimaryKey, RawSQL ())
fetchPrimaryKey (name, Array1 columns) =
  (,rawSQL name ())
    <$> pkOnColumns (map (`rawSQL` ()) columns)

-- *** CHECKS ***

sqlGetChecks :: RawSQL () -> SQL
sqlGetChecks tableName = toSQLCommand . sqlSelect "pg_catalog.pg_constraint c" $ do
  sqlResult "c.conname::text"
  sqlResult
    "regexp_replace(pg_get_constraintdef(c.oid, true), \
    \'CHECK \\((.*)\\)', '\\1') AS body" -- check body
  sqlResult "c.convalidated" -- validated?
  sqlWhereEq "c.contype" 'c'
  sqlWhereEqSql "c.conrelid" $ sqlGetTableID tableName

fetchTableCheck :: (Text, Text, Bool) -> Check
fetchTableCheck (name, condition, validated) =
  Check
    { chkName = rawSQL name ()
    , chkCondition = rawSQL condition ()
    , chkValidated = validated
    }

-- *** INDEXES ***
sqlGetIndexes :: RawSQL () -> Maybe (RawSQL ()) -> SQL
sqlGetIndexes tableName mname = toSQLCommand . sqlSelect "pg_catalog.pg_class c" $ do
  sqlResult "c.relname::text" -- index name
  sqlResult $ "ARRAY(" <> selectCoordinates "0" "i.indnkeyatts" <> ")" -- array of key columns in the index
  sqlResult $ "ARRAY(" <> selectCoordinates "i.indnkeyatts" "i.indnatts" <> ")" -- array of included columns in the index
  sqlResult "am.amname::text" -- the method used (btree, gin etc)
  sqlResult "i.indisunique" -- is it unique?
  sqlResult "i.indnullsnotdistinct" -- does it have NULLS NOT DISTINCT?
  -- if partial, get constraint def
  sqlResult "pg_catalog.pg_get_expr(i.indpred, i.indrelid, true)"
  sqlJoinOn "pg_catalog.pg_index i" "c.oid = i.indexrelid"
  sqlJoinOn "pg_catalog.pg_am am" "c.relam = am.oid"
  sqlLeftJoinOn
    "pg_catalog.pg_constraint r"
    "r.conrelid = i.indrelid AND r.conindid = i.indexrelid"
  -- Indexes currently being built by CREATE INDEX CONCURRENTLY (or ones that
  -- weren't successfully built by it) are marked as invalid, so we don't want
  -- to consider them at a time.
  sqlWhere "i.indisvalid"
  sqlWhereEqSql "i.indrelid" $ sqlGetTableID tableName
  sqlWhereIsNULL "r.contype" -- fetch only "pure" indexes
  forM_ mname $ sqlWhereEq "c.relname" . unRawSQL
  where
    -- Get all coordinates of the index.
    selectCoordinates start end =
      smconcat
        [ "WITH RECURSIVE coordinates(k, name) AS ("
        , "  VALUES (" <> start <> "::integer, NULL)"
        , "  UNION ALL"
        , "    SELECT k+1, pg_catalog.pg_get_indexdef(i.indexrelid, k+1, true)"
        , "      FROM coordinates"
        , "     WHERE k < " <> end
        , ")"
        , "SELECT name FROM coordinates WHERE name IS NOT NULL"
        ]

fetchTableIndex
  :: (Text, Array1 Text, Array1 Text, Text, Bool, Bool, Maybe Text)
  -> (TableIndex, RawSQL ())
fetchTableIndex (name, Array1 keyColumns, Array1 includeColumns, method, unique, nullsNotDistinct, mconstraint) =
  ( TableIndex
      { idxColumns = map (indexColumn . (`rawSQL` ())) keyColumns
      , idxInclude = map (`rawSQL` ()) includeColumns
      , idxMethod = case method of
          "gin" -> GIN
          "btree" -> BTree
          _ -> error $ "unexpected index method: " ++ T.unpack method
      , idxUnique = unique
      , idxWhere = (`rawSQL` ()) <$> mconstraint
      , idxNotDistinctNulls = nullsNotDistinct
      }
  , rawSQL name ()
  )

-- *** FOREIGN KEYS ***

sqlGetForeignKeys :: RawSQL () -> SQL
sqlGetForeignKeys tableName = toSQLCommand
  . sqlSelect "pg_catalog.pg_constraint r"
  $ do
    sqlResult "r.conname::text" -- fk name
    sqlResult $
      "ARRAY(SELECT a.attname::text FROM pg_catalog.pg_attribute a JOIN ("
        <> unnestWithOrdinality "r.conkey"
        <> ") conkeys ON (a.attnum = conkeys.item) \
           \WHERE a.attrelid = r.conrelid \
           \ORDER BY conkeys.n)" -- constrained columns
    sqlResult "c.relname::text" -- referenced table
    sqlResult $
      "ARRAY(SELECT a.attname::text \
      \FROM pg_catalog.pg_attribute a JOIN ("
        <> unnestWithOrdinality "r.confkey"
        <> ") confkeys ON (a.attnum = confkeys.item) \
           \WHERE a.attrelid = r.confrelid \
           \ORDER BY confkeys.n)" -- referenced columns
    sqlResult "r.confupdtype" -- on update
    sqlResult "r.confdeltype" -- on delete
    sqlResult "r.condeferrable" -- deferrable?
    sqlResult "r.condeferred" -- initially deferred?
    sqlResult "r.convalidated" -- validated?
    sqlJoinOn "pg_catalog.pg_class c" "c.oid = r.confrelid"
    sqlWhereEqSql "r.conrelid" $ sqlGetTableID tableName
    sqlWhereEq "r.contype" 'f'
  where
    unnestWithOrdinality :: RawSQL () -> SQL
    unnestWithOrdinality arr =
      "SELECT n, "
        <> raw arr
        <> "[n] AS item FROM generate_subscripts("
        <> raw arr
        <> ", 1) AS n"

fetchForeignKey
  :: (Text, Array1 Text, Text, Array1 Text, Char, Char, Bool, Bool, Bool)
  -> (ForeignKey, RawSQL ())
fetchForeignKey
  ( name
    , Array1 columns
    , reftable
    , Array1 refcolumns
    , on_update
    , on_delete
    , deferrable
    , deferred
    , validated
    ) =
    ( ForeignKey
        { fkColumns = map (`rawSQL` ()) columns
        , fkRefTable = rawSQL reftable ()
        , fkRefColumns = map (`rawSQL` ()) refcolumns
        , fkOnUpdate = charToForeignKeyAction on_update
        , fkOnDelete = charToForeignKeyAction on_delete
        , fkDeferrable = deferrable
        , fkDeferred = deferred
        , fkValidated = validated
        }
    , rawSQL name ()
    )
    where
      charToForeignKeyAction c = case c of
        'a' -> ForeignKeyNoAction
        'r' -> ForeignKeyRestrict
        'c' -> ForeignKeyCascade
        'n' -> ForeignKeySetNull
        'd' -> ForeignKeySetDefault
        _ ->
          error $
            "fetchForeignKey: invalid foreign key action code: "
              ++ show c
