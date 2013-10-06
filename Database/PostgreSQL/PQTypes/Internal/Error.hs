{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, StandaloneDeriving #-}
module Database.PostgreSQL.PQTypes.Internal.Error (
    QueryError(..)
  , InternalError(..)
  , LibPQError(..)
  , ConversionError(..)
  , ArrayItemError(..)
  , ArrayDimensionMismatch(..)
  , RowLengthMismatch(..)
  , AffectedRowsMismatch(..)
  , throwQueryError
  , throwLibPQError
  , throwLibPQTypesError
  , rethrowWithArrayError
  ) where

import Control.Applicative
import Data.Typeable
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types

newtype QueryError = QueryError String
  deriving (Show, Typeable)

newtype InternalError = InternalError String
  deriving (Show, Typeable)

newtype LibPQError = LibPQError String
  deriving (Show, Typeable)

data ConversionError = forall e. E.Exception e => ConversionError {
  convColumn     :: !Int
, convColumnName :: !String
, convRow        :: !Int
, convError      :: !e
} deriving Typeable

deriving instance Show ConversionError

data ArrayItemError = forall e. E.Exception e => ArrayItemError {
  arrItemIndex :: !Int
, arrItemError :: !e
} deriving Typeable

deriving instance Show ArrayItemError

data ArrayDimensionMismatch = ArrayDimensionMismatch {
  arrDimExpected  :: !Int
, arrDimDelivered :: !Int
} deriving (Show, Typeable)

data RowLengthMismatch = RowLengthMismatch {
  lengthExpected  :: !Int
, lengthDelivered :: !Int
} deriving (Show, Typeable)

data AffectedRowsMismatch = AffectedRowsMismatch {
  rowsExpected  :: ![(Int, Int)]
, rowsDelivered :: !Int
} deriving (Show, Typeable)

instance E.Exception QueryError
instance E.Exception InternalError
instance E.Exception LibPQError
instance E.Exception ConversionError
instance E.Exception ArrayItemError
instance E.Exception ArrayDimensionMismatch
instance E.Exception RowLengthMismatch
instance E.Exception AffectedRowsMismatch

throwQueryError :: Ptr PGconn -> IO a
throwQueryError conn = do
  msg <- peekCString =<< c_PQerrorMessage conn
  E.throwIO . QueryError $ msg

throwLibPQError :: Ptr PGconn -> String -> IO a
throwLibPQError conn ctx = do
  msg <- peekCString =<< c_PQerrorMessage conn
  E.throwIO . LibPQError
    $ if null ctx then msg else ctx ++ ": " ++ msg

throwLibPQTypesError :: Ptr PGerror -> String -> IO a
throwLibPQTypesError err ctx = do
  msg <- pgErrorMsg <$> peek err
  E.throwIO . LibPQError
    $ if null ctx then msg else ctx ++ ": " ++ msg

rethrowWithArrayError :: CInt -> E.SomeException -> IO a
rethrowWithArrayError i (E.SomeException e) =
  E.throwIO ArrayItemError {
    arrItemIndex = fromIntegral i + 1
  , arrItemError = e
  }
