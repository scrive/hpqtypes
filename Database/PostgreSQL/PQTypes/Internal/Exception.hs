{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, StandaloneDeriving #-}
module Database.PostgreSQL.PQTypes.Internal.Exception (
    DBException(..)
  , rethrowWithContext
  ) where

import Data.Typeable
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.Internal.SQL

data DBException = forall e. E.Exception e => DBException {
  dbeQueryContext :: !SQL
, dbeError        :: !e
} deriving Typeable

deriving instance Show DBException

instance E.Exception DBException

rethrowWithContext :: SQL -> E.SomeException -> IO a
rethrowWithContext ctx (E.SomeException e) = E.throwIO DBException {
    dbeQueryContext = ctx
  , dbeError = e
  }
