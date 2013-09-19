{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, DeriveFunctor, FlexibleInstances
  , RecordWildCards, ScopedTypeVariables, TypeFamilies #-}
module Database.PostgreSQL.PQTypes.Array (
    Array1D(..)
  , unArray1D
  , CompositeArray1D(..)
  , unCompositeArray1D
  ) where

import Control.Applicative
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as V

import Database.PostgreSQL.PQTypes.Composite
import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.C.Get
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Put
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.Row
import Database.PostgreSQL.PQTypes.ToSQL

newtype Array1D a = Array1D [a]
  deriving (Eq, Functor, Ord, Show)

unArray1D :: Array1D a -> [a]
unArray1D (Array1D a) = a

instance PQFormat t => PQFormat (Array1D t) where
  pqFormat _ = pqFormat (undefined::t) `BS.append` BS.pack "[]"

instance FromSQL t => FromSQL (Array1D t) where
  type PQBase (Array1D t) = PGarray
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just PGarray{..}) = flip E.finally (c_PQclear pgArrayRes) $
    if pgArrayNDims /= 1
      then E.throwIO ArrayDimensionMismatch {
          arrDimExpected = 1
        , arrDimDelivered = fromIntegral pgArrayNDims
        }
      else do
        let fmt = pqFormat (undefined::t)
        size <- c_PQntuples pgArrayRes
        alloca $ BS.useAsCString fmt . loop [] (size - 1)
    where
      loop acc (-1) _ _ = return . Array1D $ acc
      loop acc !i ptr fmt = do
        verifyPQTRes "fromSQL (Array1D)" =<< c_PQgetf1 pgArrayRes i fmt 0 ptr
        isNull <- c_PQgetisnull pgArrayRes i 0
        mbase <- if isNull == 1 then return Nothing else Just <$> peek ptr
        item <- fromSQL mbase `E.catch` rethrowWithArrayError i
        loop (item : acc) (i-1) ptr fmt

instance ToSQL t => ToSQL (Array1D t) where
  type PQDest (Array1D t) = Ptr PGarray
  toSQL (Array1D arr) allocParam conv = alloca $ \ptr -> allocParam $ \param -> do
    BS.useAsCString (pqFormat (undefined::t)) $ \fmt -> forM_ arr $ \item ->
      toSQL item allocParam (c_PQPutfMaybe param fmt)
        >>= verifyPQTRes "toSQL (Array1D)"
    poke ptr PGarray {
      pgArrayNDims = 0
    , pgArrayLBound = V.empty
    , pgArrayDims = V.empty
    , pgArrayParam = param
    , pgArrayRes = nullPtr
    }
    conv . Just $ ptr

----------------------------------------

newtype CompositeArray1D a = CompositeArray1D [a]
  deriving (Eq, Functor, Ord, Show)

unCompositeArray1D :: CompositeArray1D a -> [a]
unCompositeArray1D (CompositeArray1D a) = a

instance PQFormat t => PQFormat (CompositeArray1D t) where
  pqFormat _ = pqFormat (undefined::Array1D t)

instance CompositeFromSQL t => FromSQL (CompositeArray1D t) where
  type PQBase (CompositeArray1D t) = PGarray
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just PGarray{..}) = flip E.finally (c_PQclear pgArrayRes) $
    if pgArrayNDims /= 1
      then E.throwIO ArrayDimensionMismatch {
          arrDimExpected = 1
        , arrDimDelivered = fromIntegral pgArrayNDims
        }
      else do
        let fmt = rowFormat (undefined::CompositeRow t)
        size <- c_PQntuples pgArrayRes
        BS.useAsCString fmt (loop [] $ size - 1)
    where
      loop acc (-1) _ = return . CompositeArray1D $ acc
      loop acc !i fmt = do
        item <- (parseRow pgArrayRes i fmt >>= fromRow)
          `E.catch` rethrowWithArrayError i
        loop (item : acc) (i-1) fmt

instance CompositeToSQL t => ToSQL (CompositeArray1D t) where
  type PQDest (CompositeArray1D t) = Ptr PGarray
  toSQL (CompositeArray1D arr) allocParam conv =
    alloca $ \ptr -> allocParam $ \param -> do
      BS.useAsCString (pqFormat (undefined::t)) $ \fmt -> forM_ arr $ \item ->
        toSQL (Composite item) allocParam (c_PQPutfMaybe param fmt)
          >>= verifyPQTRes "toSQL (CompositeArray1D)"
      poke ptr PGarray {
        pgArrayNDims = 0
      , pgArrayLBound = V.empty
      , pgArrayDims = V.empty
      , pgArrayParam = param
      , pgArrayRes = nullPtr
      }
      conv . Just $ ptr
