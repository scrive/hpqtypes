{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleInstances
  , FunctionalDependencies, ScopedTypeVariables, UndecidableInstances #-}
module Database.PostgreSQL.PQTypes.Composite where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import Foreign.Ptr
import qualified Control.Exception as E
import qualified Data.ByteString as BS

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Put
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Row
import Database.PostgreSQL.PQTypes.ToSQL

newtype Composite a = Composite a
  deriving (Eq, Functor, Ord, Show)

unComposite :: Composite a -> a
unComposite (Composite a) = a

data CompositeField = forall dest base. (ToSQL dest base, PQPut base) => CF dest

class CompositeFormat dest where
  pqCompositeFormat :: dest -> ByteString

class CompositeFormat dest => CompositeFromSQL row dest | dest -> row where
  fromRow :: row -> IO dest

class CompositeFormat dest => CompositeToSQL dest where
  compositeFields :: dest -> IO [CompositeField]

instance (
    CompositeFromSQL row dest
  , Row base row
  ) => FromSQL (Ptr PGresult) (Composite dest) where
    pqFormatGet _ = pqCompositeFormat (undefined::dest)
    fromSQL Nothing = unexpectedNULL
    fromSQL (Just res) = Composite
      <$> E.finally (parseRow' res 0 >>= fromRow) (c_PQclear res)

instance CompositeToSQL dest => ToSQL (Composite dest) (Ptr PGparam) where
  pqFormatPut _ = pqCompositeFormat (undefined::dest)
  toSQL conn (Composite dest) conv = withPGparam conn $ \param -> do
    fields <- compositeFields dest
    forM_ fields $ \(CF field) -> do
      success <- toSQL conn field $ \mbase ->
        BS.useAsCString (pqFormatPut field) $ \fmt ->
          c_PQPutfMaybe param fmt mbase
      verifyPQTRes "toSQL (Composite)" success
    conv . Just $ param
