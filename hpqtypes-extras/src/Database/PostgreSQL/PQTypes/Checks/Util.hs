{-# LANGUAGE CPP #-}

module Database.PostgreSQL.PQTypes.Checks.Util
  ( ValidationResult
  , validationError
  , validationInfo
  , mapValidationResult
  , validationErrorsToInfos
  , resultCheck
  , resultHasErrors
  , topMessage
  , tblNameText
  , tblNameString
  , checkEquality
  , checkNames
  , checkPKPresence
  , objectHasLess
  , objectHasMore
  , arrListTable
  , checkOverlappingIndexesQuery
  ) where

import Control.Monad.Catch
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif
import Data.List qualified as L
import Data.Monoid.Utils
import Data.Semigroup qualified as SG
import Data.Text (Text)
import Data.Text qualified as T
import Log
import TextShow

import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Model

-- | A (potentially empty) list of info/error messages.
data ValidationResult = ValidationResult
  { vrInfos :: [Text]
  , vrErrors :: [Text]
  }
  deriving (Show, Eq)

validationError :: Text -> ValidationResult
validationError err = mempty {vrErrors = [err]}

validationInfo :: Text -> ValidationResult
validationInfo msg = mempty {vrInfos = [msg]}

-- | Downgrade all error messages in a ValidationResult to info messages.
validationErrorsToInfos :: ValidationResult -> ValidationResult
validationErrorsToInfos ValidationResult {..} =
  mempty {vrInfos = vrInfos <> vrErrors}

mapValidationResult
  :: ([Text] -> [Text]) -> ([Text] -> [Text]) -> ValidationResult -> ValidationResult
mapValidationResult mapInfos mapErrs ValidationResult {..} =
  mempty {vrInfos = mapInfos vrInfos, vrErrors = mapErrs vrErrors}

instance SG.Semigroup ValidationResult where
  (ValidationResult infos0 errs0) <> (ValidationResult infos1 errs1) =
    ValidationResult (infos0 <> infos1) (errs0 <> errs1)

instance Monoid ValidationResult where
  mempty = ValidationResult [] []
  mappend = (SG.<>)

topMessage :: Text -> Text -> ValidationResult -> ValidationResult
topMessage objtype objname vr@ValidationResult {..} =
  case vrErrors of
    [] -> vr
    es ->
      ValidationResult
        vrInfos
        ( "There are problems with the"
            <+> objtype
            <+> "'"
            <> objname
            <> "'"
            : es
        )

resultHasErrors :: ValidationResult -> Bool
resultHasErrors ValidationResult {..} = not $ null vrErrors

-- | Log all messages in a 'ValidationResult', and fail if any of them
-- were errors.
resultCheck
  :: (MonadLog m, MonadThrow m)
  => ValidationResult
  -> m ()
resultCheck ValidationResult {..} = do
  mapM_ logInfo_ vrInfos
  case vrErrors of
    [] -> return ()
    msgs -> do
      mapM_ logAttention_ msgs
      error "resultCheck: validation failed"

----------------------------------------

tblNameText :: Table -> Text
tblNameText = unRawSQL . tblName

tblNameString :: Table -> String
tblNameString = T.unpack . tblNameText

checkEquality :: (Eq t, Show t) => Text -> [t] -> [t] -> ValidationResult
checkEquality pname defs props = case (defs L.\\ props, props L.\\ defs) of
  ([], []) -> mempty
  (def_diff, db_diff) ->
    validationError $
      mconcat
        [ "Table and its definition have diverged and have "
        , showt $ length db_diff
        , " and "
        , showt $ length def_diff
        , " different "
        , pname
        , " each, respectively:\n"
        , "  ● table:"
        , showDiff db_diff
        , "\n  ● definition:"
        , showDiff def_diff
        ]
  where
    showDiff = mconcat . map (("\n    ○ " <>) . T.pack . show)

checkNames :: Show t => (t -> RawSQL ()) -> [(t, RawSQL ())] -> ValidationResult
checkNames prop_name = mconcat . map check
  where
    check (prop, name) = case prop_name prop of
      pname
        | pname == name -> mempty
        | otherwise ->
            validationError . mconcat $
              [ "Property "
              , T.pack $ show prop
              , " has invalid name (expected: "
              , unRawSQL pname
              , ", given: "
              , unRawSQL name
              , ")."
              ]

-- | Check presence of primary key on the named table. We cover all the cases so
-- this could be used standalone, but note that the those where the table source
-- definition and the table in the database differ in this respect is also
-- covered by @checkEquality@.
checkPKPresence
  :: RawSQL ()
  -- ^ The name of the table to check for presence of primary key
  -> Maybe PrimaryKey
  -- ^ A possible primary key gotten from the table data structure
  -> Maybe (PrimaryKey, RawSQL ())
  -- ^ A possible primary key as retrieved from database along
  -- with its name
  -> ValidationResult
checkPKPresence tableName mdef mpk =
  case (mdef, mpk) of
    (Nothing, Nothing) -> valRes [noSrc, noTbl]
    (Nothing, Just _) -> valRes [noSrc]
    (Just _, Nothing) -> valRes [noTbl]
    _ -> mempty
  where
    noSrc = "no source definition"
    noTbl = "no table definition"
    valRes msgs =
      validationError . mconcat $
        [ "Table "
        , unRawSQL tableName
        , " has no primary key defined "
        , " (" <> mintercalate ", " msgs <> ")"
        ]

objectHasLess :: Show t => Text -> Text -> t -> Text
objectHasLess otype ptype missing =
  otype
    <+> "in the database has *less*"
    <+> ptype
    <+> "than its definition (missing:"
    <+> T.pack (show missing)
    <> ")"

objectHasMore :: Show t => Text -> Text -> t -> Text
objectHasMore otype ptype extra =
  otype
    <+> "in the database has *more*"
    <+> ptype
    <+> "than its definition (extra:"
    <+> T.pack (show extra)
    <> ")"

arrListTable :: RawSQL () -> Text
arrListTable tableName = " ->" <+> unRawSQL tableName <> ": "

checkOverlappingIndexesQuery :: RawSQL () -> SQL
checkOverlappingIndexesQuery tableName =
  smconcat
    [ "WITH"
    , -- get predicates (WHERE clause) definition in text format (ugly but the parsed version
      -- can differ even if the predicate is the same), ignore functional indexes at the same time
      -- as that would make this query very ugly
      "     indexdata1 AS (SELECT *"
    , "                         , ((regexp_match(pg_get_indexdef(i.indexrelid)"
    , "                                        , 'WHERE (.*)$')))[1] AS preddef"
    , "                    FROM pg_index i JOIN pg_class c ON i.indexrelid = c.oid"
    , "                    WHERE i.indexprs IS NULL"
    , "                    AND NOT c.relname ILIKE 'local_%'"
    , -- Use to_regclass instead of '...'::regclass: both respect the search
      -- path, but the cast throws if the table doesn't exist whereas
      -- to_regclass returns NULL, so the query yields no rows (e.g. when
      -- checking an invalid schema with missing tables).
      "                    AND i.indrelid = to_regclass('" <> raw tableName <> "'))"
    , -- add the rest of metadata and do the join
      "   , indexdata2 AS (SELECT t1.*"
    , "                         , pg_get_indexdef(t1.indexrelid) AS contained"
    , "                         , pg_get_indexdef(t2.indexrelid) AS contains"
    , "                         , array_to_string(t1.indkey, '+') AS colindex"
    , "                         , array_to_string(t2.indkey, '+') AS colotherindex"
    , "                         , t2.indexrelid AS other_index"
    , "                         , t2.indisunique AS other_indisunique"
    , "                         , t2.preddef AS other_preddef"
    , -- cross join all indexes on the same table to try all combination (except oneself)
      "                    FROM indexdata1 AS t1"
    , "                         INNER JOIN indexdata1 AS t2 ON t1.indrelid = t2.indrelid"
    , "                                                    AND t1.indexrelid <> t2.indexrelid)"
    , "  SELECT contained"
    , "       , contains"
    , "  FROM indexdata2"
    , -- The indexes are the same or the "other" is larger than us
      "  WHERE (colotherindex = colindex"
    , "      OR colotherindex LIKE colindex || '+%')"
    , -- and we have the same predicate
      "    AND other_preddef IS NOT DISTINCT FROM preddef"
    , -- and either the other is unique (so better than us) or none of us is unique
      "    AND (NOT indisunique)"
    , "    OR ("
    , "             indisunique"
    , "         AND other_indisunique"
    , "         AND colindex = colotherindex"
    , ");"
    ]
