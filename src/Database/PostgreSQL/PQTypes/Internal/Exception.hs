{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, StandaloneDeriving #-}
module Database.PostgreSQL.PQTypes.Internal.Exception (
    DBException(..)
  , rethrowWithContext
  ) where

import Data.Typeable
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.SQL.Class

data DBException = forall e sql. (E.Exception e, Show sql) => DBException {
  dbeQueryContext :: !sql
, dbeError        :: !e
} deriving Typeable

deriving instance Show DBException

instance E.Exception DBException

rethrowWithContext :: IsSQL sql => sql -> E.SomeException -> IO a
rethrowWithContext sql (E.SomeException e) = E.throwIO DBException {
    dbeQueryContext = sql
  , dbeError = e
  }
