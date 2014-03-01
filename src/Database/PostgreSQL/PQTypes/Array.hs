{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveFunctor
  , FlexibleInstances, RecordWildCards, ScopedTypeVariables
  , TypeFamilies #-}
module Database.PostgreSQL.PQTypes.Array (
    -- * Array1
    Array1(..)
  , unArray1
  -- * CompositeArray1
  , CompositeArray1(..)
  , unCompositeArray1
  -- * Array2
  , Array2(..)
  , unArray2
  -- * CompositeArray2
  , CompositeArray2(..)
  , unCompositeArray2
  ) where

import Control.Applicative
import Control.Monad
import Data.Typeable
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as V

import Database.PostgreSQL.PQTypes.Composite
import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.C.Get
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Put
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.ToSQL

-- | One dimensional array of non-composite elements.
newtype Array1 a = Array1 [a]
  deriving (Eq, Functor, Ord, Show, Typeable)

-- | Extract list of elements from 'Array1'.
unArray1 :: Array1 a -> [a]
unArray1 (Array1 a) = a

instance PQFormat t => PQFormat (Array1 t) where
  pqFormat _ = pqFormat (undefined::t) `BS.append` BS.pack "[]"

instance FromSQL t => FromSQL (Array1 t) where
  type PQBase (Array1 t) = PGarray
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just arr) = getArray1 Array1 arr ffmt getItem
    where
      ffmt = pqFormat (undefined::t)
      getItem res err i ptr fmt = do
        verifyPQTRes err "fromSQL (Array1)" =<< c_PQgetf1 res err i fmt 0 ptr
        isNull <- c_PQgetisnull res i 0
        mbase <- if isNull == 1 then return Nothing else Just <$> peek ptr
        fromSQL mbase

instance ToSQL t => ToSQL (Array1 t) where
  type PQDest (Array1 t) = PGarray
  toSQL (Array1 arr) allocParam conv = alloca $ \err -> allocParam $ \param ->
    putArray1 arr param conv $ \fmt item ->
      toSQL item allocParam (c_PQputf1 param err fmt)
        >>= verifyPQTRes err "toSQL (Array1)"

----------------------------------------

-- | One dimensional array of composite elements.
newtype CompositeArray1 a = CompositeArray1 [a]
  deriving (Eq, Functor, Ord, Show, Typeable)

-- | Extract list of elements from 'CompositeArray1'.
unCompositeArray1 :: CompositeArray1 a -> [a]
unCompositeArray1 (CompositeArray1 a) = a

instance PQFormat t => PQFormat (CompositeArray1 t) where
  pqFormat _ = pqFormat (undefined::Array1 t)

instance CompositeFromSQL t => FromSQL (CompositeArray1 t) where
  type PQBase (CompositeArray1 t) = PGarray
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just arr) = getArray1 CompositeArray1 arr ffmt getItem
    where
      ffmt = pqFormat (undefined::CompositeRow t)
      getItem res err i (_::Ptr CInt) fmt = fromRow res err i fmt >>= toComposite

instance CompositeToSQL t => ToSQL (CompositeArray1 t) where
  type PQDest (CompositeArray1 t) = PGarray
  toSQL (CompositeArray1 arr) allocParam conv = alloca $ \err -> allocParam $ \param ->
    putArray1 arr param conv $ \fmt item ->
      toSQL (Composite item) allocParam (c_PQputf1 param err fmt)
        >>= verifyPQTRes err "toSQL (CompositeArray1)"

----------------------------------------

-- | Helper function for putting elements of
-- 'Array1' / 'CompositeArray1' into 'PGparam'.
putArray1 :: forall t r. PQFormat t
          => [t] -- ^ List of items to be put.
          -> Ptr PGparam -- ^ Inner 'PGparam' to put items into.
          -> (Ptr PGarray -> IO r) -- ^ Continuation that puts
          -- 'PGarray' into outer 'PGparam'.
          -> (CString -> t -> IO ()) -- ^ Function that takes item
          -- along with its format and puts it into inner 'PGparam'.
          -> IO r
putArray1 arr param conv putItem = do
  BS.useAsCString (pqFormat (undefined::t)) $ forM_ arr . putItem
  put (PGarray {
    pgArrayNDims = 0
  , pgArrayLBound = V.empty
  , pgArrayDims = V.empty
  , pgArrayParam = param
  , pgArrayRes = nullPtr
  }) conv

-- | Helper function for getting elements of
-- 'Array1' / 'CompositeArray1' out of 'PGarray'.
getArray1 :: forall a array t. Storable a
          => ([t] -> array) -- ^ Array constructor.
          -> PGarray -- ^ Source 'PGarray'.
          -> BS.ByteString -- ^ Format for getting an item from 'PGarray'.
          -> (Ptr PGresult -> Ptr PGerror -> CInt -> Ptr a -> CString -> IO t) -- ^
          -- Function that takes an item with a given index
          -- out of 'PGresult' and stores it in provided 'Ptr'.
          -> IO array
getArray1 con PGarray{..} ffmt getItem = flip E.finally (c_PQclear pgArrayRes) $
  if pgArrayNDims > 1
    then E.throwIO ArrayDimensionMismatch {
        arrDimExpected = 1
      , arrDimDelivered = fromIntegral pgArrayNDims
      }
    else do
      size <- c_PQntuples pgArrayRes
      alloca $ \err -> alloca $ \ptr ->
        BS.useAsCString ffmt $ loop [] (size - 1) err ptr
  where
    loop :: [t] -> CInt -> Ptr PGerror -> Ptr a -> CString -> IO array
    loop acc !i err ptr fmt = case i of
      -1 -> return . con $ acc
      _  -> do
        item <- getItem pgArrayRes err i ptr fmt `E.catch` rethrowWithArrayError i
        loop (item : acc) (i - 1) err ptr fmt

----------------------------------------

-- | Two dimensional array of non-composite elements.
newtype Array2 a = Array2 [[a]]
  deriving (Eq, Functor, Ord, Show, Typeable)

-- | Extract list of elements from 'Array2'.
unArray2 :: Array2 a -> [[a]]
unArray2 (Array2 a) = a

instance PQFormat t => PQFormat (Array2 t) where
  pqFormat _ = pqFormat (undefined::Array1 t)

instance FromSQL t => FromSQL (Array2 t) where
  type PQBase (Array2 t) = PGarray
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just arr) = getArray2 Array2 arr ffmt getItem
    where
      ffmt = pqFormat (undefined::t)
      getItem res err i ptr fmt = do
        verifyPQTRes err "fromSQL (Array2)" =<< c_PQgetf1 res err i fmt 0 ptr
        isNull <- c_PQgetisnull res i 0
        mbase <- if isNull == 1 then return Nothing else Just <$> peek ptr
        fromSQL mbase

instance ToSQL t => ToSQL (Array2 t) where
  type PQDest (Array2 t) = PGarray
  toSQL (Array2 arr) allocParam conv = alloca $ \err -> allocParam $ \param ->
    putArray2 arr param conv $ \fmt item ->
      toSQL item allocParam (c_PQputf1 param err fmt)
          >>= verifyPQTRes err "toSQL (Array2)"

----------------------------------------

-- | Two dimensional array of composite elements.
newtype CompositeArray2 a = CompositeArray2 [[a]]
  deriving (Eq, Functor, Ord, Show, Typeable)

-- | Extract list of elements from 'CompositeArray2'.
unCompositeArray2 :: CompositeArray2 a -> [[a]]
unCompositeArray2 (CompositeArray2 a) = a

instance PQFormat t => PQFormat (CompositeArray2 t) where
  pqFormat _ = pqFormat (undefined::Array2 t)

instance CompositeFromSQL t => FromSQL (CompositeArray2 t) where
  type PQBase (CompositeArray2 t) = PGarray
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just arr) = getArray2 CompositeArray2 arr ffmt getItem
    where
      ffmt = pqFormat (undefined::CompositeRow t)
      getItem res err i (_::Ptr CInt) fmt = fromRow res err i fmt >>= toComposite

instance CompositeToSQL t => ToSQL (CompositeArray2 t) where
  type PQDest (CompositeArray2 t) = PGarray
  toSQL (CompositeArray2 arr) allocParam conv = alloca $ \err -> allocParam $ \param ->
    putArray2 arr param conv $ \fmt item ->
      toSQL (Composite item) allocParam (c_PQputf1 param err fmt)
        >>= verifyPQTRes err "toSQL (CompositeArray2)"

----------------------------------------

-- | Helper function for putting elements of
-- 'Array2' / 'CompositeArray2' into 'PGparam'.
putArray2 :: forall t r. PQFormat t
          => [[t]] -- ^ List of items to be put.
          -> Ptr PGparam -- ^ Inner 'PGparam' to put items into.
          -> (Ptr PGarray -> IO r) -- ^ Continuation
          -- that puts 'PGarray' into outer 'PGparam'.
          -> (CString -> t -> IO ()) -- ^ Function that takes item
          -- along with its format and puts it into inner 'PGparam'.
          -> IO r
putArray2 arr param conv putItem = do
  dims <- BS.useAsCString (pqFormat (undefined::t)) $ loop arr 0 0
  put (PGarray {
    pgArrayNDims = 2
  , pgArrayLBound = V.fromList [1, 1]
  , pgArrayDims = dims
  , pgArrayParam = param
  , pgArrayRes = nullPtr
  }) conv
  where
    loop :: [[t]] -> CInt -> CInt -> CString -> IO (V.Vector CInt)
    loop rows !size !innerSize fmt = case rows of
      []           -> return . V.fromList $ [size, innerSize]
      (row : rest) -> do
        nextInnerSize <- innLoop row 0 fmt
        when (size > 0 && innerSize /= nextInnerSize) $
          hpqTypesError $ "putArray2: inner rows have different sizes"
        loop rest (size + 1) nextInnerSize fmt

    innLoop :: [t] -> CInt -> CString -> IO CInt
    innLoop items !size fmt = case items of
      []            -> return size
      (item : rest) -> do
        putItem fmt item
        innLoop rest (size + 1) fmt

-- | Helper function for getting elements of
-- 'Array2' / 'CompositeArray2' out of 'PGarray'.
getArray2 :: forall a array t. Storable a
          => ([[t]] -> array) -- ^ Array constructor.
          -> PGarray -- ^ Source 'PGarray'.
          -> BS.ByteString -- ^ Format for getting an item from 'PGarray'.
          -> (Ptr PGresult -> Ptr PGerror -> CInt -> Ptr a -> CString -> IO t) -- ^
          -- Function that takes an item with a given index
          -- out of 'PGresult' and stores it in provided 'Ptr'.
          -> IO array
getArray2 con PGarray{..} ffmt getItem = flip E.finally (c_PQclear pgArrayRes) $ do
  if pgArrayNDims /= 0 && pgArrayNDims /= 2
    then E.throwIO ArrayDimensionMismatch {
        arrDimExpected = 2
      , arrDimDelivered = fromIntegral pgArrayNDims
      }
    else do
      let dim2 = pgArrayDims V.! 1
      size <- c_PQntuples pgArrayRes
      alloca $ \ptr -> alloca $ \err ->
        BS.useAsCString ffmt $ loop [] dim2 size err ptr
  where
    loop :: [[t]] -> CInt -> CInt -> Ptr PGerror -> Ptr a -> CString -> IO array
    loop acc dim2 !i err ptr fmt = case i of
      0 -> return . con $ acc
      _ -> do
        let i' = i - dim2
        arr <- innLoop [] (dim2 - 1) i' err ptr fmt
        loop (arr : acc) dim2 i' err ptr fmt

    innLoop :: [t] -> CInt -> CInt -> Ptr PGerror -> Ptr a -> CString -> IO [t]
    innLoop acc !i baseIdx err ptr fmt = case i of
      -1 -> return acc
      _  -> do
        let i' = baseIdx + i
        item <- getItem pgArrayRes err i' ptr fmt `E.catch` rethrowWithArrayError i'
        innLoop (item : acc) (i - 1) baseIdx err ptr fmt
