{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module Database.PostgreSQL.PQTypes.Internal.Format where

import Data.Int
import Data.Time
import Data.Text (Text)
import Data.Word
import qualified Data.ByteString.Char8 as BS

import Database.PostgreSQL.PQTypes.Types

class PQFormat t where
  pqFormat :: t -> BS.ByteString

instance PQFormat t => PQFormat (Maybe t) where
  pqFormat _ = pqFormat (undefined::t)

instance PQFormat Int16 where
  pqFormat _ = BS.pack "%int2"

instance PQFormat Int32 where
  pqFormat _ = BS.pack "%int4"

instance PQFormat Int64 where
  pqFormat _ = BS.pack "%int8"

instance PQFormat Float where
  pqFormat _ = BS.pack "%float4"

instance PQFormat Double where
  pqFormat _ = BS.pack "%float8"

instance PQFormat t => PQFormat (Array t) where
  pqFormat _ = pqFormat (undefined::t) `BS.append` BS.pack "[]"

instance PQFormat Char where
  pqFormat _ = BS.pack "%char"

instance PQFormat Word8 where
  pqFormat _ = BS.pack "%char"

instance PQFormat String where
  pqFormat _ = BS.pack "%text"

instance PQFormat BS.ByteString where
  pqFormat _ = BS.pack "%text"

instance PQFormat Text where
  pqFormat _ = BS.pack "%text"

instance PQFormat (Binary BS.ByteString) where
  pqFormat _ = BS.pack "%bytea"

instance PQFormat Day where
  pqFormat _ = BS.pack "%date"

instance PQFormat TimeOfDay where
  pqFormat _ = BS.pack "%time"

instance PQFormat LocalTime where
  pqFormat _ = BS.pack "%timestamp"

instance PQFormat UTCTime where
  pqFormat _ = BS.pack "%timestamptz"

instance PQFormat ZonedTime where
  pqFormat _ = BS.pack "%timestamptz"
