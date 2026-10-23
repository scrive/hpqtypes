module Database.PostgreSQL.PQTypes.Model.Function where

import Data.Map.Strict qualified as M
import Data.Monoid.Utils
import Data.Text qualified as T
import Database.PostgreSQL.PQTypes

data Function = Function
  { fnName :: RawSQL ()
  -- ^ Name of the function it can be referenced with.
  , fnBody :: RawSQL ()
  -- ^ The body of the function.
  , fnReturns :: RawSQL ()
  -- ^ The return type of the function.
  , fnSecurity :: Security
  -- ^ Indicates with which privileges the function is executed with. By default
  -- this is usually the caller (INVOKER). The alternative is DEFINER, where the
  -- function is executed under the user that owns, not calls, it.
  , fnConfigurationParameters :: M.Map T.Text (RawSQL ())
  -- ^ Functions allow setting configuration parameters during the execution of
  -- the function. This corresponds to the SET clause in CREATE FUNCTION. See
  -- the following:
  --   - https://www.postgresql.org/docs/current/sql-createfunction.html
  --   - https://www.postgresql.org/docs/current/sql-set.html
  }
  deriving (Show, Eq)

data Security = Invoker | Definer
  deriving (Show, Eq)

-- | Turn the function into a SQL statement.
--
-- NB: If an existing function has the same name, it is replaced.
--
-- @since 1.20.0.0
sqlCreateFunction :: Function -> RawSQL ()
sqlCreateFunction Function {..} =
  "CREATE OR REPLACE FUNCTION"
    <+> fnName
    <> "()"
    <+> returns
    <+> "AS $$"
    <> fnBody
    <> "$$"
    <+> "LANGUAGE PLPGSQL"
    <+> "VOLATILE"
    <+> security
    <+> "RETURNS NULL ON NULL INPUT"
    <+> searchPath
    <> ";"
  where
    returns = "RETURNS" <+> fnReturns
    security = case fnSecurity of
      Invoker -> "SECURITY INVOKER"
      Definer -> "SECURITY DEFINER"
    searchPath =
      foldr
        (\(param, value) prev -> "SET" <+> unsafeSQL (T.unpack param) <+> "=" <+> value <+> prev)
        (unsafeSQL "")
        $ M.toList fnConfigurationParameters

-- | Build an SQL statement for dropping a function.
--
-- @since 1.20.0.0
sqlDropFunction :: Function -> RawSQL ()
sqlDropFunction Function {..} =
  "DROP FUNCTION" <+> fnName <+> "RESTRICT"
