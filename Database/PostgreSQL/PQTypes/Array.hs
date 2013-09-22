{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, DeriveFunctor, FlexibleInstances
  , RecordWildCards, ScopedTypeVariables, TypeFamilies #-}
module Database.PostgreSQL.PQTypes.Array (
    Array1(..)
  , unArray1
  , CompositeArray1(..)
  , unCompositeArray1
  , Array2(..)
  , unArray2
  ) where

import Control.Applicative
import Control.Monad
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as V

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

newtype Array1 a = Array1 [a]
  deriving (Eq, Functor, Ord, Show)

unArray1 :: Array1 a -> [a]
unArray1 (Array1 a) = a

instance PQFormat t => PQFormat (Array1 t) where
  pqFormat _ = pqFormat (undefined::t) `BS.append` BS.pack "[]"

instance FromSQL t => FromSQL (Array1 t) where
  type PQBase (Array1 t) = PGarray
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
      loop acc (-1) _ _ = return . Array1 $ acc
      loop acc !i ptr fmt = do
        verifyPQTRes "fromSQL (Array1)" =<< c_PQgetf1 pgArrayRes i fmt 0 ptr
        isNull <- c_PQgetisnull pgArrayRes i 0
        mbase <- if isNull == 1 then return Nothing else Just <$> peek ptr
        item <- fromSQL mbase `E.catch` rethrowWithArrayError i
        loop (item : acc) (i-1) ptr fmt

instance ToSQL t => ToSQL (Array1 t) where
  type PQDest (Array1 t) = Ptr PGarray
  toSQL (Array1 arr) allocParam conv = alloca $ \ptr -> allocParam $ \param -> do
    BS.useAsCString (pqFormat (undefined::t)) $ \fmt -> forM_ arr $ \item ->
      toSQL item allocParam (c_PQPutfMaybe param fmt)
        >>= verifyPQTRes "toSQL (Array1)"
    poke ptr PGarray {
      pgArrayNDims = 0
    , pgArrayLBound = V.empty
    , pgArrayDims = V.empty
    , pgArrayParam = param
    , pgArrayRes = nullPtr
    }
    conv . Just $ ptr

----------------------------------------

newtype CompositeArray1 a = CompositeArray1 [a]
  deriving (Eq, Functor, Ord, Show)

unCompositeArray1 :: CompositeArray1 a -> [a]
unCompositeArray1 (CompositeArray1 a) = a

instance PQFormat t => PQFormat (CompositeArray1 t) where
  pqFormat _ = pqFormat (undefined::Array1 t)

instance CompositeFromSQL t => FromSQL (CompositeArray1 t) where
  type PQBase (CompositeArray1 t) = PGarray
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just PGarray{..}) = flip E.finally (c_PQclear pgArrayRes) $
    if pgArrayNDims > 1
      then E.throwIO ArrayDimensionMismatch {
          arrDimExpected = 1
        , arrDimDelivered = fromIntegral pgArrayNDims
        }
      else do
        let fmt = rowFormat (undefined::CompositeRow t)
        size <- c_PQntuples pgArrayRes
        BS.useAsCString fmt (loop [] $ size - 1)
    where
      loop acc (-1) _ = return . CompositeArray1 $ acc
      loop acc !i fmt = do
        item <- (parseRow pgArrayRes i fmt >>= fromRow)
          `E.catch` rethrowWithArrayError i
        loop (item : acc) (i-1) fmt

instance CompositeToSQL t => ToSQL (CompositeArray1 t) where
  type PQDest (CompositeArray1 t) = Ptr PGarray
  toSQL (CompositeArray1 arr) allocParam conv =
    alloca $ \ptr -> allocParam $ \param -> do
      BS.useAsCString (pqFormat (undefined::t)) $ \fmt -> forM_ arr $ \item ->
        toSQL (Composite item) allocParam (c_PQPutfMaybe param fmt)
          >>= verifyPQTRes "toSQL (CompositeArray1)"
      poke ptr PGarray {
        pgArrayNDims = 0
      , pgArrayLBound = V.empty
      , pgArrayDims = V.empty
      , pgArrayParam = param
      , pgArrayRes = nullPtr
      }
      conv . Just $ ptr

----------------------------------------

newtype Array2 a = Array2 [[a]]
  deriving (Eq, Functor, Ord, Show)

unArray2 :: Array2 a -> [[a]]
unArray2 (Array2 a) = a

instance PQFormat t => PQFormat (Array2 t) where
  pqFormat _ = pqFormat (undefined::Array1 t)

instance (Show t, FromSQL t) => FromSQL (Array2 t) where
  type PQBase (Array2 t) = PGarray
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just PGarray{..}) = flip E.finally (c_PQclear pgArrayRes) $ do
    if pgArrayNDims /= 0 && pgArrayNDims /= 2
      then E.throwIO ArrayDimensionMismatch {
          arrDimExpected = 2
        , arrDimDelivered = fromIntegral pgArrayNDims
        }
      else do
        let dim2 = fromIntegral $ pgArrayDims V.! 1
            fmt  = pqFormat (undefined::t)
        size <- c_PQntuples pgArrayRes
        alloca $ BS.useAsCString fmt . loop [] dim2 size
    where
      loop :: [[t]] -> CInt -> CInt -> Ptr (PQBase t) -> CString -> IO (Array2 t)
      loop acc    _  0   _   _ = return . Array2 $ acc
      loop acc dim2 !i ptr fmt = do
        let i' = i - dim2
        arr <- innLoop [] (dim2 - 1) i' ptr fmt
        loop (arr : acc) dim2 i' ptr fmt

      innLoop :: [t] -> CInt -> CInt -> Ptr (PQBase t) -> CString -> IO [t]
      innLoop acc (-1)       _   _   _ = return acc
      innLoop acc   !i baseIdx ptr fmt = do
        let i' = baseIdx + i
        verifyPQTRes "fromSQL (Array2)" =<< c_PQgetf1 pgArrayRes i' fmt 0 ptr
        isNull <- c_PQgetisnull pgArrayRes i' 0
        mbase <- if isNull == 1 then return Nothing else Just <$> peek ptr
        item <- fromSQL mbase `E.catch` rethrowWithArrayError i'
        innLoop (item : acc) (i-1) baseIdx ptr fmt

instance ToSQL t => ToSQL (Array2 t) where
  type PQDest (Array2 t) = Ptr PGarray
  toSQL (Array2 arr) allocParam conv = alloca $ \ptr -> allocParam $ \param -> do
    dims <- BS.useAsCString (pqFormat (undefined::t)) $ loop arr param 0 0
    poke ptr PGarray {
      pgArrayNDims = 2
    , pgArrayLBound = V.fromList [1, 1]
    , pgArrayDims = dims
    , pgArrayParam = param
    , pgArrayRes = nullPtr
    }
    conv . Just $ ptr
    where
      loop :: [[t]] -> Ptr PGparam -> CInt -> CInt -> CString -> IO (V.Vector CInt)
      loop [] _ !size !innerSize _ = return . V.fromList $ [size, innerSize]
      loop (row : rest) param !size !innerSize fmt = do
        nextInnerSize <- innLoop row param 0 fmt
        when (size > 0 && innerSize /= nextInnerSize) $
          E.throwIO . InternalError $ "toSQL (Array2): inner rows have different sizes"
        loop rest param (size+1) nextInnerSize fmt

      innLoop :: [t] -> Ptr PGparam -> CInt -> CString -> IO CInt
      innLoop [] _ !size _ = return size
      innLoop (item : rest) param !size fmt = do
        toSQL item allocParam (c_PQPutfMaybe param fmt)
          >>= verifyPQTRes "toSQL (Array2)"
        innLoop rest param (size+1) fmt
