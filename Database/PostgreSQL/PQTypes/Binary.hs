{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances, RecordWildCards
  , TypeFamilies #-}
module Database.PostgreSQL.PQTypes.Binary (
    Binary(..)
  , unBinary
  ) where

import Control.Applicative
import Data.ByteString.Unsafe
import qualified Data.ByteString.Char8 as BS

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.ToSQL

newtype Binary b = Binary b
  deriving (Eq, Functor, Ord, Show)

unBinary :: Binary b -> b
unBinary (Binary b) = b

instance PQFormat (Binary BS.ByteString) where
  pqFormat _ = BS.pack "%bytea"

instance FromSQL (Binary BS.ByteString) where
  type PQBase (Binary BS.ByteString) = PGbytea
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just PGbytea{..}) = Binary
    <$> BS.packCStringLen (pgByteaData, fromIntegral pgByteaLen)

instance ToSQL (Binary BS.ByteString) where
  type PQDest (Binary BS.ByteString) = PGbytea
  toSQL (Binary bs) _ conv = unsafeUseAsCStringLen bs $ \cslen ->
    put (cStringLenToBytea cslen) conv
