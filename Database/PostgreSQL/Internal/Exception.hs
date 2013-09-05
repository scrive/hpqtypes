{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Database.PostgreSQL.Internal.Exception where

import Data.Typeable
import Control.Exception
import Foreign.C.String
import Foreign.Ptr

import Database.PostgreSQL.Internal.C.Interface
import Database.PostgreSQL.Internal.C.Types
import Database.PostgreSQL.Internal.SQL

data DBException = DBException {
  dbeQueryContext :: SQL
, dbeError :: Error
} deriving (Show, Typeable)

data Error
  = LibPQError {
    libPQMessage :: String
  }
  | InternalError {
    internalMessage :: String
  }
  | ConversionError {
    convColumn :: Int
  , convColumnName :: String
  , convRow :: Int
  , convError :: String
  }
  | RowLengthMismatch {
    lengthExpected :: Int
  , lengthDelivered :: Int
  }
  | AffectedRowsMismatch {
    rowsExpected :: Int
  , rowsAffected :: Int
  } deriving Show

instance Exception DBException

throwLibPQError :: Ptr PGconn -> SQL -> IO a
throwLibPQError conn ctx = do
  msg <- peekCString =<< c_PQerrorMessage conn
  throwIO DBException {
    dbeQueryContext = ctx
  , dbeError = LibPQError msg
  }

throwLibPQTypesError :: SQL -> IO a
throwLibPQTypesError ctx = do
  msg <- peekCString =<< c_PQgeterror
  throwIO DBException {
    dbeQueryContext = ctx
  , dbeError = LibPQError msg
  }

throwInternalError :: SQL -> String -> IO a
throwInternalError ctx msg = throwIO DBException {
    dbeQueryContext = ctx
  , dbeError = InternalError msg
  }

throwConversionError :: SQL -> Int -> String -> Int -> String -> IO a
throwConversionError ctx col colname row errmsg = throwIO DBException {
    dbeQueryContext = ctx
  , dbeError = ConversionError {
      convColumn = col
    , convColumnName = colname
    , convRow = row
    , convError = errmsg
    }
  }

throwRowLengthMismatch :: SQL -> Int -> Int -> IO a
throwRowLengthMismatch ctx expected delivered = throw DBException {
    dbeQueryContext = ctx
  , dbeError = RowLengthMismatch {
      lengthExpected = expected
    , lengthDelivered = delivered
    }
  }
