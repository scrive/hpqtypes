{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances, ScopedTypeVariables
  , TypeFamilies, UndecidableInstances #-}
module Database.PostgreSQL.PQTypes.FromRow (
    FromRow(..)
  , fromRow'
  ) where

import Control.Applicative
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import qualified Control.Exception as E
import qualified Data.ByteString as BS

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.C.Get
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Utils

u :: a
u = undefined

convert :: FromSQL t => Ptr PGresult -> CInt -> CInt -> PQBase t -> IO t
convert res tuple column base = do
  isNull <- c_PQgetisnull res tuple column
  fromSQL (if isNull == 1 then Nothing else Just base) `E.catch` rethrowWithConvError
  where
    rethrowWithConvError :: E.SomeException -> IO a
    rethrowWithConvError (E.SomeException e) = do
      colname <- peekCString =<< c_PQfname res column
      E.throwIO ConversionError {
        convColumn = fromIntegral column + 1
      , convColumnName = colname
      , convRow = fromIntegral tuple + 1
      , convError = e
      }

verify :: Ptr PGerror -> CInt -> IO ()
verify err = verifyPQTRes err "fromRow"

----------------------------------------

fromRow' :: forall row. FromRow row => Ptr PGresult -> CInt -> IO row
fromRow' res i = alloca $ \err ->
  BS.useAsCString (rowFormat (u::row)) (fromRow res err i)

class FromRow row where
  fromRow  :: Ptr PGresult -> Ptr PGerror -> CInt -> CString -> IO row
  rowFormat :: row -> BS.ByteString
  rowLength :: row -> Int

instance FromSQL t => FromRow t where
  fromRow res err i fmt = alloca $ \p1 -> do
    verify err =<< c_PQgetf1 res err i fmt 0 p1
    peek p1 >>= convert res i 0

  rowFormat = pqFormat
  rowLength _ = 1

instance (
    FromSQL t1, FromSQL t2
  ) => FromRow (t1, t2) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> do
        verify err =<< c_PQgetf2 res err i fmt 0 p0 1 p1
        (,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)

    rowFormat _ = BS.concat [pqFormat (u::t1), pqFormat (u::t2)]
    rowLength _ = 2

instance (
    FromSQL t1, FromSQL t2, FromSQL t3
  ) => FromRow (t1, t2, t3) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> do
        verify err =<< c_PQgetf3 res err i fmt 0 p0 1 p1 2 p2
        (,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3)
      ]
    rowLength _ = 3

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4
  ) => FromRow (t1, t2, t3, t4) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> do
        verify err =<< c_PQgetf4 res err i fmt 0 p0 1 p1 2 p2 3 p3
        (,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3)
      , pqFormat (u::t4)
      ]
    rowLength _ = 4

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5
  ) => FromRow (t1, t2, t3, t4, t5) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 -> do
        verify err =<< c_PQgetf5 res err i fmt 0 p0 1 p1 2 p2 3 p3 4 p4
        (,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3)
      , pqFormat (u::t4), pqFormat (u::t5)
      ]
    rowLength _ = 5

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  ) => FromRow (t1, t2, t3, t4, t5, t6) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> do
        verify err =<< c_PQgetf6 res err i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5
        (,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6)
      ]
    rowLength _ = 6

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> do
        verify err =<< c_PQgetf7 res err i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6
        (,,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)
          <*> (peek p6 >>= convert res i 6)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7)
      ]
    rowLength _ = 7

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> do
        verify err =<< c_PQgetf8 res err i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7
        (,,,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)
          <*> (peek p6 >>= convert res i 6)
          <*> (peek p7 >>= convert res i 7)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      ]
    rowLength _ = 8

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> do
        verify err =<< c_PQgetf9 res err i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8
        (,,,,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)
          <*> (peek p6 >>= convert res i 6)
          <*> (peek p7 >>= convert res i 7)
          <*> (peek p8 >>= convert res i 8)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9)
      ]
    rowLength _ = 9

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 -> do
        verify err =<< c_PQgetf10 res err i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9
        (,,,,,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)
          <*> (peek p6 >>= convert res i 6)
          <*> (peek p7 >>= convert res i 7)
          <*> (peek p8 >>= convert res i 8)
          <*> (peek p9 >>= convert res i 9)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10)
      ]
    rowLength _ = 10

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> do
        verify err =<< c_PQgetf11 res err i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10
        (,,,,,,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)
          <*> (peek p6 >>= convert res i 6)
          <*> (peek p7 >>= convert res i 7)
          <*> (peek p8 >>= convert res i 8)
          <*> (peek p9 >>= convert res i 9)
          <*> (peek p10 >>= convert res i 10)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11)
      ]
    rowLength _ = 11

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> do
        verify err =<< c_PQgetf12 res err i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11
        (,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)
          <*> (peek p6 >>= convert res i 6)
          <*> (peek p7 >>= convert res i 7)
          <*> (peek p8 >>= convert res i 8)
          <*> (peek p9 >>= convert res i 9)
          <*> (peek p10 >>= convert res i 10)
          <*> (peek p11 >>= convert res i 11)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      ]
    rowLength _ = 12

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> do
        verify err =<< c_PQgetf13 res err i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12
        (,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)
          <*> (peek p6 >>= convert res i 6)
          <*> (peek p7 >>= convert res i 7)
          <*> (peek p8 >>= convert res i 8)
          <*> (peek p9 >>= convert res i 9)
          <*> (peek p10 >>= convert res i 10)
          <*> (peek p11 >>= convert res i 11)
          <*> (peek p12 >>= convert res i 12)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13)
      ]
    rowLength _ = 13

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> do
        verify err =<< c_PQgetf14 res err i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13
        (,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)
          <*> (peek p6 >>= convert res i 6)
          <*> (peek p7 >>= convert res i 7)
          <*> (peek p8 >>= convert res i 8)
          <*> (peek p9 >>= convert res i 9)
          <*> (peek p10 >>= convert res i 10)
          <*> (peek p11 >>= convert res i 11)
          <*> (peek p12 >>= convert res i 12)
          <*> (peek p13 >>= convert res i 13)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14)
      ]
    rowLength _ = 14

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 -> do
        verify err =<< c_PQgetf15 res err i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14
        (,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)
          <*> (peek p6 >>= convert res i 6)
          <*> (peek p7 >>= convert res i 7)
          <*> (peek p8 >>= convert res i 8)
          <*> (peek p9 >>= convert res i 9)
          <*> (peek p10 >>= convert res i 10)
          <*> (peek p11 >>= convert res i 11)
          <*> (peek p12 >>= convert res i 12)
          <*> (peek p13 >>= convert res i 13)
          <*> (peek p14 >>= convert res i 14)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15)
      ]
    rowLength _ = 15

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> do
        verify err =<< c_PQgetf16 res err i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15
        (,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)
          <*> (peek p6 >>= convert res i 6)
          <*> (peek p7 >>= convert res i 7)
          <*> (peek p8 >>= convert res i 8)
          <*> (peek p9 >>= convert res i 9)
          <*> (peek p10 >>= convert res i 10)
          <*> (peek p11 >>= convert res i 11)
          <*> (peek p12 >>= convert res i 12)
          <*> (peek p13 >>= convert res i 13)
          <*> (peek p14 >>= convert res i 14)
          <*> (peek p15 >>= convert res i 15)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      ]
    rowLength _ = 16

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> do
        verify err =<< c_PQgetf17 res err i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15 16 p16
        (,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)
          <*> (peek p6 >>= convert res i 6)
          <*> (peek p7 >>= convert res i 7)
          <*> (peek p8 >>= convert res i 8)
          <*> (peek p9 >>= convert res i 9)
          <*> (peek p10 >>= convert res i 10)
          <*> (peek p11 >>= convert res i 11)
          <*> (peek p12 >>= convert res i 12)
          <*> (peek p13 >>= convert res i 13)
          <*> (peek p14 >>= convert res i 14)
          <*> (peek p15 >>= convert res i 15)
          <*> (peek p16 >>= convert res i 16)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17)
      ]
    rowLength _ = 17

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> do
        verify err =<< c_PQgetf18 res err i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15 16 p16 17 p17
        (,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)
          <*> (peek p6 >>= convert res i 6)
          <*> (peek p7 >>= convert res i 7)
          <*> (peek p8 >>= convert res i 8)
          <*> (peek p9 >>= convert res i 9)
          <*> (peek p10 >>= convert res i 10)
          <*> (peek p11 >>= convert res i 11)
          <*> (peek p12 >>= convert res i 12)
          <*> (peek p13 >>= convert res i 13)
          <*> (peek p14 >>= convert res i 14)
          <*> (peek p15 >>= convert res i 15)
          <*> (peek p16 >>= convert res i 16)
          <*> (peek p17 >>= convert res i 17)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18)
      ]
    rowLength _ = 18

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> do
        verify err =<< c_PQgetf19 res err i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15 16 p16 17 p17 18 p18
        (,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)
          <*> (peek p6 >>= convert res i 6)
          <*> (peek p7 >>= convert res i 7)
          <*> (peek p8 >>= convert res i 8)
          <*> (peek p9 >>= convert res i 9)
          <*> (peek p10 >>= convert res i 10)
          <*> (peek p11 >>= convert res i 11)
          <*> (peek p12 >>= convert res i 12)
          <*> (peek p13 >>= convert res i 13)
          <*> (peek p14 >>= convert res i 14)
          <*> (peek p15 >>= convert res i 15)
          <*> (peek p16 >>= convert res i 16)
          <*> (peek p17 >>= convert res i 17)
          <*> (peek p18 >>= convert res i 18)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19)
      ]
    rowLength _ = 19

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) where
    fromRow res err i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 -> do
        verify err =<< c_PQgetf20 res err i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15 16 p16 17 p17 18 p18 19 p19
        (,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)
          <*> (peek p6 >>= convert res i 6)
          <*> (peek p7 >>= convert res i 7)
          <*> (peek p8 >>= convert res i 8)
          <*> (peek p9 >>= convert res i 9)
          <*> (peek p10 >>= convert res i 10)
          <*> (peek p11 >>= convert res i 11)
          <*> (peek p12 >>= convert res i 12)
          <*> (peek p13 >>= convert res i 13)
          <*> (peek p14 >>= convert res i 14)
          <*> (peek p15 >>= convert res i 15)
          <*> (peek p16 >>= convert res i 16)
          <*> (peek p17 >>= convert res i 17)
          <*> (peek p18 >>= convert res i 18)
          <*> (peek p19 >>= convert res i 19)

    rowFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      ]
    rowLength _ = 20
