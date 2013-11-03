{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, StandaloneDeriving #-}
module Database.PostgreSQL.PQTypes.SQL.Raw (
    RawSQL
  , rawSQL
  ) where

import Data.Monoid
import Data.String
import Foreign.Marshal.Alloc
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.ToRow

data RawSQL row = RawSQL !BS.ByteString !row
  deriving (Eq, Ord, Show)

instance IsString (RawSQL ()) where
  fromString = flip RawSQL () . T.encodeUtf8 . T.pack

instance (Show row, ToRow row) => IsSQL (RawSQL row) where
  someSQL = SomeSQL
  withSQL (RawSQL query row) allocParam execute = alloca $ \err ->
    allocParam $ \param -> BS.useAsCString (pqFormat row) $ \fmt -> do
      toRow row allocParam param err fmt
      BS.useAsCString query (execute param)

instance Monoid (RawSQL ()) where
  mempty = rawSQL BS.empty ()
  RawSQL a () `mappend` RawSQL b () = RawSQL (a `mappend` b) ()

rawSQL :: (Show row, ToRow row) => BS.ByteString -> row -> RawSQL row
rawSQL = RawSQL
