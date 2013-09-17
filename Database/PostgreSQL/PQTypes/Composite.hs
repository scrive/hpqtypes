{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts
  , FlexibleInstances, ScopedTypeVariables, TypeFamilies #-}
module Database.PostgreSQL.PQTypes.Composite where

import Control.Applicative
import Control.Monad
import Foreign.Ptr
import qualified Control.Exception as E
import qualified Data.ByteString as BS

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Put
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Format
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Row
import Database.PostgreSQL.PQTypes.ToSQL

newtype Composite a = Composite a
  deriving (Eq, Functor, Ord, Show)

instance PQFormat t => PQFormat (Composite t) where
  pqFormat _ = pqFormat (undefined::t)

unComposite :: Composite a -> a
unComposite (Composite a) = a

data CompositeField = forall t. ToSQL t => CF t

class (PQFormat t, Row (CompositeRow t)) => CompositeFromSQL t where
  type CompositeRow t
  fromRow :: CompositeRow t -> IO t

class CompositeToSQL t where
  compositeFields :: t -> IO [CompositeField]

instance CompositeFromSQL t => FromSQL (Composite t) where
  type PQBase (Composite t) = Ptr PGresult
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just res) = Composite
    <$> E.finally (parseRow' res 0 >>= fromRow) (c_PQclear res)

instance (CompositeToSQL t, PQFormat t) => ToSQL (Composite t) where
  type PQDest (Composite t) = Ptr PGparam
  toSQL conn (Composite comp) conv = withPGparam conn $ \param -> do
    fields <- compositeFields comp
    forM_ fields $ \(CF field) -> do
      success <- toSQL conn field $ \mbase ->
        BS.useAsCString (pqFormat field) $ \fmt ->
          c_PQPutfMaybe param fmt mbase
      verifyPQTRes "toSQL (Composite)" success
    conv . Just $ param
