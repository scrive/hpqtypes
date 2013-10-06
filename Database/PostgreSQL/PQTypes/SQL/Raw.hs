{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}
module Database.PostgreSQL.PQTypes.SQL.Raw (
    RawSQL
  , rawSQL
  ) where

import Data.String
import Foreign.Marshal.Alloc
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.ToRow

data RawSQL = forall row. (Show row, ToRow row) => RawSQL !BS.ByteString !row

deriving instance Show RawSQL

instance IsString RawSQL where
  fromString = flip RawSQL () . T.encodeUtf8 . T.pack

instance IsSQL RawSQL where
  someSQL = SomeSQL
  withSQL (RawSQL query row) allocParam execute = alloca $ \err ->
    allocParam $ \param -> BS.useAsCString (pqFormat row) $ \fmt -> do
      toRow row allocParam param err fmt
      BS.useAsCString query (execute param)

rawSQL :: (Show row, ToRow row) => BS.ByteString -> row -> RawSQL
rawSQL = RawSQL
