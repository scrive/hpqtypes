{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, FlexibleInstances
  , RecordWildCards, TypeFamilies #-}
module Database.PostgreSQL.PQTypes.Binary (
    Binary(..)
  , unBinary
  ) where

import Control.Applicative
import Data.Typeable
import qualified Data.ByteString.Char8 as BS

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.ToSQL

-- | Wrapper for (de)serializing underlying type as 'bytea'.
newtype Binary b = Binary b
  deriving (Eq, Functor, Ord, Show, Typeable)

-- | Extract underlying value.
unBinary :: Binary b -> b
unBinary (Binary b) = b

instance PQFormat (Binary BS.ByteString) where
  pqFormat _ = BS.pack "%bytea"

instance FromSQL (Binary BS.ByteString) where
  type PQBase (Binary BS.ByteString) = PGbytea
  -- use handler for ByteStringS for convenience
  fromSQL mbase = Binary <$> fromSQL mbase

instance ToSQL (Binary BS.ByteString) where
  type PQDest (Binary BS.ByteString) = PGbytea
  -- use handler for ByteStringS as it properly handles
  -- the case when underlying ByteString pointer is NULL.
  toSQL (Binary bs) = toSQL bs
