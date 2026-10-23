module Database.PostgreSQL.PQTypes.Model.Domain
  ( Domain (..)
  , mkChecks
  , sqlCreateDomain
  , sqlAlterDomain
  , sqlDropDomain
  ) where

import Data.Monoid.Utils
import Data.Set (Set, fromList)
import Database.PostgreSQL.PQTypes

import Database.PostgreSQL.PQTypes.Model.Check
import Database.PostgreSQL.PQTypes.Model.ColumnType

-- Domains are global, i.e. not bound to any particular table.
-- The first table that uses a new domain needs to create it
-- by a migration.
--
-- If a migration that alters the domain needs to be performed,
-- there are three possible situations:
--
-- 1) The modification doesn't require data change in any of the tables.
-- 2) The modification requires data change, but only in one table.
-- 3) The modification requires data change in more than one table.
--
-- These situations should be handled as follows:
--
-- 1) One of the tables that use the domain should migrate it.
-- 2) The table that requires data modification should migrate it.
-- 3) One of the tables that require data modification should migrate
-- it.  Note that modification of constraints may conflict with the
-- data in the other tables. In this case, these constraints should be
-- created as NOT VALID (see
-- http://www.postgresql.org/docs/current/static/sql-alterdomain.html
-- for more info) and VALIDATEd in the migration of the last table
-- with the conflicting data.
--
-- TODO: the proper solution to this is to version the domains to be
-- able to handle (1) and the first and last part of (3) by migrating
-- the domain itself, however that requires substantial change to the
-- migration system.
--
-- As opposed to the current solution, the other temporary one is to
-- create domains statically and not worry about migrations. The problem
-- with this approach is the separation of domain creation from the rest
-- of the universe, which results in problems later, when the proper
-- solution will have to be implemented (i.e. one would need to go back
-- and edit old migrations), whereas the current solution makes the
-- transition trivial.

data Domain = Domain
  { domName :: RawSQL ()
  -- ^ Name of the domain.
  , domType :: ColumnType
  -- ^ Type of the domain.
  , domNullable :: Bool
  -- ^ Defines whether the domain value can be NULL.
  -- *Cannot* be superseded by a table column definition.
  , -- Default value for the domain. *Can* be
    -- superseded by a table column definition.
    domDefault :: Maybe (RawSQL ())
  , -- Set of constraint checks on the domain.
    domChecks :: Set Check
  }
  deriving (Eq, Ord, Show)

mkChecks :: [Check] -> Set Check
mkChecks = fromList

sqlCreateDomain :: Domain -> RawSQL ()
sqlCreateDomain Domain {..} =
  smconcat
    [ "CREATE DOMAIN" <+> domName <+> "AS"
    , columnTypeToSQL domType
    , if domNullable then "NULL" else "NOT NULL"
    , maybe "" ("DEFAULT" <+>) domDefault
    ]

sqlAlterDomain :: RawSQL () -> RawSQL () -> RawSQL ()
sqlAlterDomain dname alter = "ALTER DOMAIN" <+> dname <+> alter

sqlDropDomain :: RawSQL () -> RawSQL ()
sqlDropDomain dname = "DROP DOMAIN" <+> dname
