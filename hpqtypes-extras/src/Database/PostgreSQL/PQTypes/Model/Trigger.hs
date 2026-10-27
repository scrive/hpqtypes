-- |
-- Module: Database.PostgreSQL.PQTypes.Model.Trigger
--
-- Trigger name must be unique among triggers of the same table.
--
-- For details, see <https://www.postgresql.org/docs/current/sql-createtrigger.html>.
module Database.PostgreSQL.PQTypes.Model.Trigger
  ( -- * Triggers
    TriggerKind (..)
  , TriggerActionTime (..)
  , TriggerEvent (..)
  , Trigger (..)
  , triggerMakeName
  , triggerBaseName
  , sqlCreateTrigger
  , sqlDropTrigger
  , createTrigger
  , dropTrigger
  , getDBTriggers

    -- * Trigger functions
  , defaultTriggerFunction
  , sqlCreateTriggerFunction
  , sqlDropTriggerFunction
  , triggerFunctionMakeName

    -- * Constraints
  , ConstraintAttributes (..)
  ) where

import Data.Bits (testBit)
import Data.Foldable qualified as F
import Data.Int
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid.Utils
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Model.Function
import Database.PostgreSQL.PQTypes.SQL.Builder

-- | Timing for a regular trigger.
--
-- @since 1.17.0.0
data TriggerActionTime
  = -- | An @AFTER@ trigger.
    After
  | -- | A @BEFORE@ trigger.
    Before
  deriving (Eq, Show)

-- | Possible combinations of constraint attributes.
--
-- @since 1.17.0.0
data ConstraintAttributes
  = -- | The @NOT DEFERRABLE [INITIALLY IMMEDIATE]@ constraint.
    -- A @NOT DEFERRABLE@ constraint is @INITIALLY IMMEDIATE@ by default.
    NotDeferrable
  | -- | The @DEFERRABLE [INITIALLY IMMEDIATE]@ constraint.
    -- A @DEFERRABLE@ constraint is @INITIALLY IMMEDIATE@ by default.
    Deferrable
  | -- | The @DEFERRABLE INITIALLY DEFERRED@ constraint.
    DeferrableInitiallyDeferred
  deriving (Eq, Show)

-- | Trigger kind.
--
-- @since 1.17.0.0
data TriggerKind
  = -- | Create a regular trigger: @CREATE TRIGGER@
    TriggerRegular TriggerActionTime
  | -- | Create a constraint trigger: @CREATE CONSTRAINT TRIGGER@
    TriggerConstraint ConstraintAttributes
  deriving (Eq, Show)

-- | Trigger event name.
--
-- @since 1.15.0.0
data TriggerEvent
  = -- | The @INSERT@ event.
    TriggerInsert
  | -- | The @UPDATE@ event.
    TriggerUpdate
  | -- | The @UPDATE OF column1 [, column2 ...]@ event.
    TriggerUpdateOf [RawSQL ()]
  | -- | The @DELETE@ event.
    TriggerDelete
  deriving (Eq, Ord, Show)

-- | Trigger.
--
-- @since 1.15.0.0
data Trigger = Trigger
  { triggerTable :: RawSQL ()
  -- ^ The table that the trigger is associated with.
  , triggerName :: RawSQL ()
  -- ^ The internal name without any prefixes. Trigger name must be unique among
  -- triggers of same table. See 'triggerMakeName'.
  , triggerKind :: TriggerKind
  -- ^ The kind of trigger we want to create.
  -- @since 1.17.0.0
  , triggerEvents :: Set TriggerEvent
  -- ^ The set of events. Corresponds to the @{ __event__ [ OR ... ] }@ in the trigger
  -- definition. The order in which they are defined doesn't matter and there can
  -- only be one of each.
  , triggerWhen :: Maybe (RawSQL ())
  -- ^ The condition that specifies whether the trigger should fire. Corresponds to the
  -- @WHEN ( __condition__ )@ in the trigger definition.
  , triggerFunction :: Function
  -- ^ The function to execute when the trigger fires.
  -- The function is declared as taking no arguments and returning type @trigger@.
  }
  deriving (Show, Eq)

-- Function source code is not guaranteed to be equal, so we ignore it.

-- | Make a trigger name that can be used in SQL.
--
-- Given a base @name@ and @tableName@, return a new name that will be used as the
-- actual name of the trigger in an SQL query. The returned name is in the format
-- @trg\__\<tableName\>\__\<name\>@.
--
-- @since 1.15.0
triggerMakeName :: RawSQL () -> RawSQL () -> RawSQL ()
triggerMakeName name tableName = "trg__" <> tableName <> "__" <> name

-- | Return the trigger's base name.
--
-- Given the trigger's actual @name@ and @tableName@, return the base name of the
-- trigger. This is basically the reverse of what 'triggerMakeName' does.
--
-- @since 1.15.0
triggerBaseName :: RawSQL () -> RawSQL () -> RawSQL ()
triggerBaseName name tableName =
  rawSQL (snd . Text.breakOnEnd (unRawSQL tableName <> "__") $ unRawSQL name) ()

triggerEventName :: TriggerEvent -> RawSQL ()
triggerEventName = \case
  TriggerInsert -> "INSERT"
  TriggerUpdate -> "UPDATE"
  TriggerUpdateOf columns ->
    if null columns
      then error "UPDATE OF must have columns."
      else "UPDATE OF" <+> mintercalate ", " columns
  TriggerDelete -> "DELETE"

-- | Build an SQL statement that creates a trigger.
--
-- @since 1.15.0
sqlCreateTrigger :: Trigger -> RawSQL ()
sqlCreateTrigger Trigger {..} =
  "CREATE"
    <+> trgKind
    <+> trgName
    <+> tgrActionTime
    <+> trgEvents
    <+> "ON"
    <+> triggerTable
    <+> trgConstraintAttributes
    <+> "FOR EACH ROW"
    <+> trgWhen
    <+> "EXECUTE FUNCTION"
    <+> trgFunction
    <+> "()"
  where
    trgKind = case triggerKind of
      TriggerRegular _ -> "TRIGGER"
      TriggerConstraint _ -> "CONSTRAINT TRIGGER"
    trgName
      | triggerName == "" = error "Trigger must have a name."
      | otherwise = triggerMakeName triggerName triggerTable
    tgrActionTime = case triggerKind of
      TriggerRegular After -> "AFTER"
      TriggerRegular Before -> "BEFORE"
      TriggerConstraint _ -> "AFTER"
    trgEvents
      | triggerEvents == Set.empty = error "Trigger must have at least one event."
      | otherwise = mintercalate " OR " . map triggerEventName $ Set.toList triggerEvents
    trgConstraintAttributes = case triggerKind of
      TriggerRegular _ -> ""
      TriggerConstraint NotDeferrable -> "NOT DEFERRABLE"
      TriggerConstraint Deferrable -> "DEFERRABLE"
      TriggerConstraint DeferrableInitiallyDeferred -> "DEFERRABLE INITIALLY DEFERRED"
    trgWhen = maybe "" (\w -> "WHEN (" <+> w <+> ")") triggerWhen
    trgFunction = fnName triggerFunction

-- | Build an SQL statement that drops a trigger.
--
-- @since 1.15.0
sqlDropTrigger :: Trigger -> RawSQL ()
sqlDropTrigger Trigger {..} =
  -- In theory, because the trigger is dependent on its function, it should be enough to
  -- 'DROP FUNCTION triggerFunction CASCADE'. However, let's make this safe and go with
  -- the default RESTRICT here.
  "DROP TRIGGER" <+> trgName <+> "ON" <+> triggerTable <+> "RESTRICT"
  where
    trgName
      | triggerName == "" = error "Trigger must have a name."
      | otherwise = triggerMakeName triggerName triggerTable

-- | Create the trigger in the database.
--
-- First, create the trigger's associated function, then create the trigger itself.
--
-- @since 1.15.0
createTrigger :: MonadDB m => Trigger -> m ()
createTrigger trigger = do
  runQuery_ $ sqlCreateFunction (triggerFunction trigger)
  runQuery_ $ sqlCreateTrigger trigger

-- | Drop the trigger from the database.
--
-- @since 1.15.0
dropTrigger :: MonadDB m => Trigger -> m ()
dropTrigger trigger = do
  -- First, drop the trigger, as it is dependent on the function. See the comment in
  -- 'sqlDropTrigger'.
  runQuery_ $ sqlDropTrigger trigger
  runQuery_ $ sqlDropFunction (triggerFunction trigger)

-- | Get all noninternal triggers from the database.
--
-- Run a query that returns all triggers associated with the given table and marked as
-- @tgisinternal = false@. The second item in the returned tuple is the trigger's function
-- name.
--
-- Note that, in the background, to get the trigger's @WHEN@ clause and the source code of
-- the attached function, the entire query that had created the trigger is received using
-- @pg_get_triggerdef(t.oid, true)::text@ and then parsed. The result of that call will be
-- decompiled and normalized, which means that it's likely not what the user had
-- originally typed.
--
-- @since 1.15.0
getDBTriggers :: forall m. MonadDB m => RawSQL () -> m [Trigger]
getDBTriggers tableName = do
  runQuery_ . sqlSelect "pg_trigger t" $ do
    sqlResult "t.tgname::text" -- name
    sqlResult "t.tgtype" -- smallint == int2 => (2 bytes)
    sqlResult "t.tgconstraint > 0" -- we only check if CONSTRAINT has been specified
    sqlResult "t.tgdeferrable" -- boolean
    sqlResult "t.tginitdeferred" -- boolean
    -- This gets the entire query that created this trigger. Note that it's decompiled and
    -- normalized, which means that it's likely not what the user actually typed. For
    -- example, if the original query had excessive whitespace in it, it won't be in this
    -- result.
    sqlResult "pg_get_triggerdef(t.oid, true)::text"
    sqlResult "p.proname::text"
    sqlResult "trim(p.prosrc)" -- text
    sqlResult "p.prosecdef" -- bool => true = SECURITY DEFINER, false = SECURITY INVOKER
    sqlResult "fn_rettype.typname::text"
    sqlResult "p.proconfig::text[]"
    sqlResult "c.relname::text"
    sqlJoinOn "pg_proc p" "t.tgfoid = p.oid"
    sqlJoinOn "pg_class c" "c.oid = t.tgrelid"
    sqlJoinOn "pg_type fn_rettype" "fn_rettype.oid = p.prorettype"
    sqlWhereEq "t.tgisinternal" False
    sqlWhereEq "c.relname" $ unRawSQL tableName
  fetchMany getTrigger
  where
    getTrigger :: (String, Int16, Bool, Bool, Bool, String, String, String, Bool, String, Maybe (Array1 Text), String) -> Trigger
    getTrigger (tgname, tgtype, tgconstraint, tgdeferrable, tginitdeferrable, triggerdef, proname, prosrc, prosecdef, prorettype, proconfig, tblName) =
      ( Trigger
          { triggerTable = tableName'
          , triggerName = triggerBaseName (unsafeSQL tgname) tableName'
          , triggerKind = tgrKind
          , triggerEvents = trgEvents
          , triggerWhen = tgrWhen
          , triggerFunction =
              Function
                { fnName = unsafeSQL proname
                , fnBody = unsafeSQL prosrc
                , fnReturns = unsafeSQL prorettype
                , fnSecurity =
                    if prosecdef
                      then Definer
                      else Invoker
                , fnConfigurationParameters =
                    M.fromList . mapMaybe parseFnConfigurationParameter $
                      maybe [] unArray1 proconfig
                }
          }
      )
      where
        -- Parses "config_name=value" strings into tuples ("config_name", value)
        parseFnConfigurationParameter s
          | [configName, value] <- T.splitOn "=" s =
              Just (configName, unsafeSQL $ T.unpack value)
          | otherwise = Nothing

        tableName' :: RawSQL ()
        tableName' = unsafeSQL tblName

        parseBetween :: Text -> Text -> Maybe (RawSQL ())
        parseBetween left right =
          let (prefix, match) = Text.breakOnEnd left $ Text.pack triggerdef
          in if Text.null prefix
               then Nothing
               else Just $ (rawSQL . fst $ Text.breakOn right match) ()

        -- Get the WHEN part of the query. Anything between WHEN and EXECUTE is what we
        -- want. The Postgres' grammar guarantees that WHEN and EXECUTE are always next to
        -- each other and in that order.
        tgrWhen :: Maybe (RawSQL ())
        tgrWhen = parseBetween "WHEN (" ") EXECUTE"

        tgrKind :: TriggerKind
        tgrKind =
          if tgconstraint
            then TriggerConstraint trgConstraintAttrs
            else TriggerRegular tgrActionTime

        trgConstraintAttrs :: ConstraintAttributes
        trgConstraintAttrs = case (tgdeferrable, tginitdeferrable) of
          (False, False) -> NotDeferrable
          (True, False) -> Deferrable
          (True, True) -> DeferrableInitiallyDeferred
          (False, True) -> error "A constraint declared INITIALLY DEFERRED must be DEFERRABLE."

        tgrActionTime :: TriggerActionTime
        tgrActionTime = case (tgtypeInsteadBit, tgtypeBeforeBit) of
          (False, False) -> After
          (False, True) -> Before
          (True, False) -> error "INSTEAD OF triggers are not available on tables."
          (True, True) -> error "The tgtype can't match more than one timing."
          where
            -- Taken from PostgreSQL sources: src/include/catalog/pg_trigger.h:
            tgtypeBeforeBit = testBit tgtype 1 -- #define TRIGGER_TYPE_BEFORE (1 << 1)
            tgtypeInsteadBit = testBit tgtype 6 -- #define TRIGGER_TYPE_INSTEAD (1 << 6)

        -- Similarly, in case of UPDATE OF, the columns can be simply parsed from the
        -- original query. Note that UPDATE and UPDATE OF are mutually exclusive and have
        -- the same bit set in the underlying tgtype bit field.
        trgEvents :: Set TriggerEvent
        trgEvents =
          F.foldl'
            ( \set (mask, event) ->
                if testBit tgtype mask
                  then
                    Set.insert
                      ( if event == TriggerUpdate
                          then maybe event trgUpdateOf $ parseBetween "UPDATE OF " " ON"
                          else event
                      )
                      set
                  else set
            )
            Set.empty
            -- Taken from PostgreSQL sources: src/include/catalog/pg_trigger.h:
            [ (2, TriggerInsert) -- #define TRIGGER_TYPE_INSERT (1 << 2)
            , (3, TriggerDelete) -- #define TRIGGER_TYPE_DELETE (1 << 3)
            , (4, TriggerUpdate) -- #define TRIGGER_TYPE_UPDATE (1 << 4)
            ]

        trgUpdateOf :: RawSQL () -> TriggerEvent
        trgUpdateOf columnsSQL =
          let columns = map (unsafeSQL . Text.unpack) . Text.splitOn ", " $ unRawSQL columnsSQL
          in TriggerUpdateOf columns

-- | Build an SQL statement for creating a trigger function.
--
-- Since we only support @CONSTRAINT@ triggers, the function will always @RETURN TRIGGER@
-- and will have no parameters.
--
-- @since 1.15.0.0
sqlCreateTriggerFunction :: Trigger -> RawSQL ()
sqlCreateTriggerFunction Trigger {..} =
  sqlCreateFunction triggerFunction

-- | Backwards compatible helper for using pre-1.20.0.0 trigger functions
--
-- @since 1.20.0.0
defaultTriggerFunction :: RawSQL () -> RawSQL () -> Function
defaultTriggerFunction triggerName functionBody =
  Function
    { fnName = triggerFunctionMakeName triggerName
    , fnBody = functionBody
    , fnReturns = "trigger"
    , fnSecurity = Invoker
    , fnConfigurationParameters = mempty
    }

-- | Build an SQL statement for dropping a trigger function.
--
-- @since 1.15.0.0
sqlDropTriggerFunction :: Trigger -> RawSQL ()
sqlDropTriggerFunction Trigger {..} =
  "DROP FUNCTION" <+> triggerFunctionMakeName triggerName <+> "RESTRICT"

-- | Make a trigger function name that can be used in SQL.
--
-- Given a base @name@, return a new name that will be used as the actual name
-- of the trigger function in an SQL query. The returned name is in the format
-- @trgfun\__\<name\>@.
--
-- @since 1.16.0.0
triggerFunctionMakeName :: RawSQL () -> RawSQL ()
triggerFunctionMakeName name = "trgfun__" <> name
