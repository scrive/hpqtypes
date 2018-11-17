{-# LANGUAGE TypeApplications #-}
module Database.PostgreSQL.PQTypes.Composite (
    Composite(..)
  , unComposite
  , CompositeRow
  , CompositeFromSQL(..)
  , CompositeToSQL(..)
  ) where

import Data.Typeable
import Foreign.Ptr
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.ToRow
import Database.PostgreSQL.PQTypes.ToSQL

-- | Wrapper for (de)serializing composite types.
newtype Composite a = Composite a
  deriving (Eq, Functor, Ord, Show, Typeable)

-- | Extract underlying value.
unComposite :: Composite a -> a
unComposite (Composite a) = a

-- | Type function which maps composite type to its intermediate
-- representation as a tuple (row) of Haskell types that correspond
-- to PostgreSQL types in composite type definition.
--
-- As an example, consider the type defined as (a INTEGER, b DATE).
-- Then its CompositeRow instance could be (Maybe Int32, Maybe Day),
-- (Maybe Int32, Day), (Int32, Maybe Day) or (Int32, Day).
type family CompositeRow t :: *

-- | Class which represents \"from SQL to composite\" transformation.
class (PQFormat t, FromRow (CompositeRow t)) => CompositeFromSQL t where
  -- | Convert composite row to destination type.
  toComposite :: CompositeRow t -> t

-- | Class which represents \"from composite to SQL\" transformation.
class (PQFormat t, ToRow (CompositeRow t)) => CompositeToSQL t where
  -- | Convert composite type to its intermediate representation.
  fromComposite :: t -> CompositeRow t

instance PQFormat t => PQFormat (Composite t) where
  pqFormat = pqFormat @t

instance CompositeFromSQL t => FromSQL (Composite t) where
  type PQBase (Composite t) = Ptr PGresult
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just res) = Composite
    <$> E.finally (toComposite <$> fromRow' res 0 0) (c_PQclear res)

instance CompositeToSQL t => ToSQL (Composite t) where
  type PQDest (Composite t) = PGparam
  toSQL (Composite comp) pa@(ParamAllocator allocParam) conv =
    allocParam $ \param -> do
      toRow' (fromComposite comp) pa param
      conv param
