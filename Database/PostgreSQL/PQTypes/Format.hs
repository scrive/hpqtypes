{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module Database.PostgreSQL.PQTypes.Format (
    PQFormat(..)
  ) where

import Data.Int
import Data.Time
import Data.Text (Text)
import Data.Word
import qualified Data.ByteString.Char8 as BS

class PQFormat t where
  pqFormat :: t -> BS.ByteString

-- NULLables

instance PQFormat t => PQFormat (Maybe t) where
  pqFormat _ = pqFormat (undefined::t)

-- NUMERICS

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

-- CHAR

instance PQFormat Char where
  pqFormat _ = BS.pack "%char"

instance PQFormat Word8 where
  pqFormat _ = BS.pack "%char"

-- VARIABLE-LENGTH CHARACTER TYPES

instance PQFormat String where
  pqFormat _ = BS.pack "%text"

instance PQFormat BS.ByteString where
  pqFormat _ = BS.pack "%text"

instance PQFormat Text where
  pqFormat _ = BS.pack "%text"

-- DATE

instance PQFormat Day where
  pqFormat _ = BS.pack "%date"

-- TIME

instance PQFormat TimeOfDay where
  pqFormat _ = BS.pack "%time"

-- TIMESTAMP

instance PQFormat LocalTime where
  pqFormat _ = BS.pack "%timestamp"

-- TIMESTAMPTZ

instance PQFormat UTCTime where
  pqFormat _ = BS.pack "%timestamptz"

instance PQFormat ZonedTime where
  pqFormat _ = BS.pack "%timestamptz"
