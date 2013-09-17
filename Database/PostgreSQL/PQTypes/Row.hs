{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, ScopedTypeVariables
  , UndecidableInstances #-}
module Database.PostgreSQL.PQTypes.Row (
    Row(..)
  , parseRow'
  ) where

import Control.Applicative
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import qualified Control.Exception as E
import qualified Data.ByteString as BS

import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.C.Get
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Utils

u :: a
u = undefined

convert :: FromSQL base dest => Ptr PGresult -> CInt -> CInt -> base -> IO dest
convert res tuple column base = do
  isNull <- c_PQgetisnull res tuple column
  fromSQL (if isNull == 1 then Nothing else Just base) `E.catch` nestError
  where
    nestError :: E.SomeException -> IO a
    nestError (E.SomeException e) = do
      colname <- peekCString =<< c_PQfname res column
      E.throwIO ConversionError {
        convColumn = fromIntegral column + 1
      , convColumnName = colname
      , convRow = fromIntegral tuple + 1
      , convError = e
      }

verify :: CInt -> IO ()
verify = verifyPQTRes "parseRow"

----------------------------------------

parseRow' :: forall base dest. Row base dest => Ptr PGresult -> CInt -> IO dest
parseRow' res i = BS.useAsCString (rowFormat (u::dest)) (parseRow res i)

class Row base dest | dest -> base where
  parseRow  :: Ptr PGresult -> CInt -> CString -> IO dest
  rowFormat :: dest -> BS.ByteString
  rowLength :: dest -> Int

instance FromSQL base dest => Row base dest where
  parseRow res i fmt = alloca $ \p1 -> do
    verify =<< c_PQgetf1 res i fmt 0 p1
    peek p1 >>= convert res i 0

  rowFormat = pqFormatGet
  rowLength _ = 1

instance (
    FromSQL b1 d1, FromSQL b2 d2
  ) => Row (b1, b2)
           (d1, d2) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> do
        verify =<< c_PQgetf2 res i fmt 0 p0 1 p1
        (,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)

    rowFormat _ = BS.concat [pqFormatGet (u::d1), pqFormatGet (u::d2)]
    rowLength _ = 2

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3
  ) => Row (b1, b2, b3)
           (d1, d2, d3) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> do
        verify =<< c_PQgetf3 res i fmt 0 p0 1 p1 2 p2
        (,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)

    rowFormat _ = BS.concat [
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      ]
    rowLength _ = 3

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4
  ) => Row (b1, b2, b3, b4)
           (d1, d2, d3, d4) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> do
        verify =<< c_PQgetf4 res i fmt 0 p0 1 p1 2 p2 3 p3
        (,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)

    rowFormat _ = BS.concat [
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4)
      ]
    rowLength _ = 4

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  ) => Row (b1, b2, b3, b4, b5)
           (d1, d2, d3, d4, d5) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 -> do
        verify =<< c_PQgetf5 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4
        (,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)

    rowFormat _ = BS.concat [
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4), pqFormatGet (u::d5)
      ]
    rowLength _ = 5

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6
  ) => Row (b1, b2, b3, b4, b5, b6)
           (d1, d2, d3, d4, d5, d6) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> do
        verify =<< c_PQgetf6 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5
        (,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)

    rowFormat _ = BS.concat [
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4), pqFormatGet (u::d5), pqFormatGet (u::d6)
      ]
    rowLength _ = 6

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7
  ) => Row (b1, b2, b3, b4, b5, b6, b7)
           (d1, d2, d3, d4, d5, d6, d7) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> do
        verify =<< c_PQgetf7 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6
        (,,,,,,)
          <$> (peek p0 >>= convert res i 0)
          <*> (peek p1 >>= convert res i 1)
          <*> (peek p2 >>= convert res i 2)
          <*> (peek p3 >>= convert res i 3)
          <*> (peek p4 >>= convert res i 4)
          <*> (peek p5 >>= convert res i 5)
          <*> (peek p6 >>= convert res i 6)

    rowFormat _ = BS.concat [
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4), pqFormatGet (u::d5), pqFormatGet (u::d6)
      , pqFormatGet (u::d7)
      ]
    rowLength _ = 7

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7, FromSQL b8 d8
  ) => Row (b1, b2, b3, b4, b5, b6, b7, b8)
           (d1, d2, d3, d4, d5, d6, d7, d8) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> do
        verify =<< c_PQgetf8 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7
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
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4), pqFormatGet (u::d5), pqFormatGet (u::d6)
      , pqFormatGet (u::d7), pqFormatGet (u::d8)
      ]
    rowLength _ = 8

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7, FromSQL b8 d8, FromSQL b9 d9
  ) => Row (b1, b2, b3, b4, b5, b6, b7, b8, b9)
           (d1, d2, d3, d4, d5, d6, d7, d8, d9) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> do
        verify =<< c_PQgetf9 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8
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
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4), pqFormatGet (u::d5), pqFormatGet (u::d6)
      , pqFormatGet (u::d7), pqFormatGet (u::d8), pqFormatGet (u::d9)
      ]
    rowLength _ = 9

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7, FromSQL b8 d8, FromSQL b9 d9, FromSQL b10 d10
  ) => Row (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10)
           (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 -> do
        verify =<< c_PQgetf10 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9
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
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4), pqFormatGet (u::d5), pqFormatGet (u::d6)
      , pqFormatGet (u::d7), pqFormatGet (u::d8), pqFormatGet (u::d9)
      , pqFormatGet (u::d10)
      ]
    rowLength _ = 10

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7, FromSQL b8 d8, FromSQL b9 d9, FromSQL b10 d10
  , FromSQL b11 d11
  ) => Row (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11)
           (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> do
        verify =<< c_PQgetf11 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10
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
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4), pqFormatGet (u::d5), pqFormatGet (u::d6)
      , pqFormatGet (u::d7), pqFormatGet (u::d8), pqFormatGet (u::d9)
      , pqFormatGet (u::d10), pqFormatGet (u::d11)
      ]
    rowLength _ = 11

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7, FromSQL b8 d8, FromSQL b9 d9, FromSQL b10 d10
  , FromSQL b11 d11, FromSQL b12 d12
  ) => Row (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12)
           (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> do
        verify =<< c_PQgetf12 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11
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
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4), pqFormatGet (u::d5), pqFormatGet (u::d6)
      , pqFormatGet (u::d7), pqFormatGet (u::d8), pqFormatGet (u::d9)
      , pqFormatGet (u::d10), pqFormatGet (u::d11), pqFormatGet (u::d12)
      ]
    rowLength _ = 12

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7, FromSQL b8 d8, FromSQL b9 d9, FromSQL b10 d10
  , FromSQL b11 d11, FromSQL b12 d12, FromSQL b13 d13
  ) => Row (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13)
           (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> do
        verify =<< c_PQgetf13 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12
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
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4), pqFormatGet (u::d5), pqFormatGet (u::d6)
      , pqFormatGet (u::d7), pqFormatGet (u::d8), pqFormatGet (u::d9)
      , pqFormatGet (u::d10), pqFormatGet (u::d11), pqFormatGet (u::d12)
      , pqFormatGet (u::d13)
      ]
    rowLength _ = 13

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7, FromSQL b8 d8, FromSQL b9 d9, FromSQL b10 d10
  , FromSQL b11 d11, FromSQL b12 d12, FromSQL b13 d13, FromSQL b14 d14
  ) => Row (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14)
           (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> do
        verify =<< c_PQgetf14 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13
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
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4), pqFormatGet (u::d5), pqFormatGet (u::d6)
      , pqFormatGet (u::d7), pqFormatGet (u::d8), pqFormatGet (u::d9)
      , pqFormatGet (u::d10), pqFormatGet (u::d11), pqFormatGet (u::d12)
      , pqFormatGet (u::d13), pqFormatGet (u::d14)
      ]
    rowLength _ = 14

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7, FromSQL b8 d8, FromSQL b9 d9, FromSQL b10 d10
  , FromSQL b11 d11, FromSQL b12 d12, FromSQL b13 d13, FromSQL b14 d14, FromSQL b15 d15
  ) => Row (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15)
           (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 -> do
        verify =<< c_PQgetf15 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14
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
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4), pqFormatGet (u::d5), pqFormatGet (u::d6)
      , pqFormatGet (u::d7), pqFormatGet (u::d8), pqFormatGet (u::d9)
      , pqFormatGet (u::d10), pqFormatGet (u::d11), pqFormatGet (u::d12)
      , pqFormatGet (u::d13), pqFormatGet (u::d14), pqFormatGet (u::d15)
      ]
    rowLength _ = 15

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7, FromSQL b8 d8, FromSQL b9 d9, FromSQL b10 d10
  , FromSQL b11 d11, FromSQL b12 d12, FromSQL b13 d13, FromSQL b14 d14, FromSQL b15 d15
  , FromSQL b16 d16
  ) => Row (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16)
           (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> do
        verify =<< c_PQgetf16 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15
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
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4), pqFormatGet (u::d5), pqFormatGet (u::d6)
      , pqFormatGet (u::d7), pqFormatGet (u::d8), pqFormatGet (u::d9)
      , pqFormatGet (u::d10), pqFormatGet (u::d11), pqFormatGet (u::d12)
      , pqFormatGet (u::d13), pqFormatGet (u::d14), pqFormatGet (u::d15)
      , pqFormatGet (u::d16)
      ]
    rowLength _ = 16

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7, FromSQL b8 d8, FromSQL b9 d9, FromSQL b10 d10
  , FromSQL b11 d11, FromSQL b12 d12, FromSQL b13 d13, FromSQL b14 d14, FromSQL b15 d15
  , FromSQL b16 d16, FromSQL b17 d17
  ) => Row (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17)
           (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> do
        verify =<< c_PQgetf17 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15 16 p16
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
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4), pqFormatGet (u::d5), pqFormatGet (u::d6)
      , pqFormatGet (u::d7), pqFormatGet (u::d8), pqFormatGet (u::d9)
      , pqFormatGet (u::d10), pqFormatGet (u::d11), pqFormatGet (u::d12)
      , pqFormatGet (u::d13), pqFormatGet (u::d14), pqFormatGet (u::d15)
      , pqFormatGet (u::d16), pqFormatGet (u::d17)
      ]
    rowLength _ = 17

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7, FromSQL b8 d8, FromSQL b9 d9, FromSQL b10 d10
  , FromSQL b11 d11, FromSQL b12 d12, FromSQL b13 d13, FromSQL b14 d14, FromSQL b15 d15
  , FromSQL b16 d16, FromSQL b17 d17, FromSQL b18 d18
  ) => Row (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18)
           (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> do
        verify =<< c_PQgetf18 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15 16 p16 17 p17
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
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4), pqFormatGet (u::d5), pqFormatGet (u::d6)
      , pqFormatGet (u::d7), pqFormatGet (u::d8), pqFormatGet (u::d9)
      , pqFormatGet (u::d10), pqFormatGet (u::d11), pqFormatGet (u::d12)
      , pqFormatGet (u::d13), pqFormatGet (u::d14), pqFormatGet (u::d15)
      , pqFormatGet (u::d16), pqFormatGet (u::d17), pqFormatGet (u::d18)
      ]
    rowLength _ = 18

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7, FromSQL b8 d8, FromSQL b9 d9, FromSQL b10 d10
  , FromSQL b11 d11, FromSQL b12 d12, FromSQL b13 d13, FromSQL b14 d14, FromSQL b15 d15
  , FromSQL b16 d16, FromSQL b17 d17, FromSQL b18 d18, FromSQL b19 d19
  ) => Row (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19)
           (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> do
        verify =<< c_PQgetf19 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15 16 p16 17 p17 18 p18
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
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4), pqFormatGet (u::d5), pqFormatGet (u::d6)
      , pqFormatGet (u::d7), pqFormatGet (u::d8), pqFormatGet (u::d9)
      , pqFormatGet (u::d10), pqFormatGet (u::d11), pqFormatGet (u::d12)
      , pqFormatGet (u::d13), pqFormatGet (u::d14), pqFormatGet (u::d15)
      , pqFormatGet (u::d16), pqFormatGet (u::d17), pqFormatGet (u::d18)
      , pqFormatGet (u::d19)
      ]
    rowLength _ = 19

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7, FromSQL b8 d8, FromSQL b9 d9, FromSQL b10 d10
  , FromSQL b11 d11, FromSQL b12 d12, FromSQL b13 d13, FromSQL b14 d14, FromSQL b15 d15
  , FromSQL b16 d16, FromSQL b17 d17, FromSQL b18 d18, FromSQL b19 d19, FromSQL b20 d20
  ) => Row (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20)
           (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19, d20) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 -> do
        verify =<< c_PQgetf20 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15 16 p16 17 p17 18 p18 19 p19
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
        pqFormatGet (u::d1), pqFormatGet (u::d2), pqFormatGet (u::d3)
      , pqFormatGet (u::d4), pqFormatGet (u::d5), pqFormatGet (u::d6)
      , pqFormatGet (u::d7), pqFormatGet (u::d8), pqFormatGet (u::d9)
      , pqFormatGet (u::d10), pqFormatGet (u::d11), pqFormatGet (u::d12)
      , pqFormatGet (u::d13), pqFormatGet (u::d14), pqFormatGet (u::d15)
      , pqFormatGet (u::d16), pqFormatGet (u::d17), pqFormatGet (u::d18)
      , pqFormatGet (u::d19), pqFormatGet (u::d20)
      ]
    rowLength _ = 20
