module Database.PostgreSQL.PQTypes.XML
  ( XML (..)
  ) where

import Data.Text
import PostgreSQL.Binary.Decoding qualified as D
import PostgreSQL.Binary.Encoding qualified as E

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.Oid
import Database.PostgreSQL.PQTypes.ToSQL

-- | Representation of SQL XML types as 'Text'.  Users of hpqtypes may
-- want to add conversion instances for their favorite XML type around 'XML'.
newtype XML = XML {unXML :: Text}
  deriving stock (Eq, Ord, Read, Show)

instance PQFormat XML where
  pqOid = xmlOid
  pqArrayOid = xmlArrayOid

instance FromSQL XML where
  fromSQL = decodeScalar $ XML <$> D.text_strict

instance ToSQL XML where
  toSQL = Just . E.text_strict . unXML
