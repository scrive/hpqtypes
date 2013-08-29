{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, OverlappingInstances
  , ScopedTypeVariables, UndecidableInstances #-}
module Database.PostgreSQL.Row (Row(..)) where

import Control.Applicative
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import qualified Data.ByteString as BS

import Database.PostgreSQL.FromSQL
import Database.PostgreSQL.Internal.C.Get
import Database.PostgreSQL.Internal.C.Interface
import Database.PostgreSQL.Internal.C.Types
import Database.PostgreSQL.Internal.Utils

u :: a
u = undefined

nullCheck :: Storable base => Ptr PGresult -> CInt -> CInt -> base -> IO (Maybe base)
nullCheck res tuple column base = do
  isNull <- c_PQgetisnull res tuple column
  return $ if isNull == 1
    then Nothing
    else Just base

----------------------------------------

class Row base dest | dest -> base where
  rowFormat :: dest -> BS.ByteString
  parseRow  :: Ptr PGresult -> CInt -> CString -> IO dest

instance FromSQL base dest => Row base dest where
  parseRow res i fmt = alloca $ \p1 -> do
    success <- c_PQgetf1 res i fmt 0 p1
    successCheck success
    peek p1 >>= nullCheck res i 0 >>= fromSQL

  rowFormat = pqTypesFormat

instance (
    FromSQL b1 d1, FromSQL b2 d2
  ) => Row (b1, b2)
           (d1, d2) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> do
        success <- c_PQgetf2 res i fmt 0 p0 1 p1
        successCheck success
        (,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)

    rowFormat _ = BS.concat [pqTypesFormat (u::d1), pqTypesFormat (u::d2)]

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3
  ) => Row (b1, b2, b3)
           (d1, d2, d3) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> do
        success <- c_PQgetf3 res i fmt 0 p0 1 p1 2 p2
        successCheck success
        (,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      ]

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4
  ) => Row (b1, b2, b3, b4)
           (d1, d2, d3, d4) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> do
        success <- c_PQgetf4 res i fmt 0 p0 1 p1 2 p2 3 p3
        successCheck success
        (,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4)
      ]

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  ) => Row (b1, b2, b3, b4, b5)
           (d1, d2, d3, d4, d5) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 -> do
        success <- c_PQgetf5 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4
        successCheck success
        (,,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)
          <*> (peek p4 >>= nullCheck res i 4 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5)
      ]

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6
  ) => Row (b1, b2, b3, b4, b5, b6)
           (d1, d2, d3, d4, d5, d6) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> do
        success <- c_PQgetf6 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5
        successCheck success
        (,,,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)
          <*> (peek p4 >>= nullCheck res i 4 >>= fromSQL)
          <*> (peek p5 >>= nullCheck res i 5 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5), pqTypesFormat (u::d6)
      ]

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7
  ) => Row (b1, b2, b3, b4, b5, b6, b7)
           (d1, d2, d3, d4, d5, d6, d7) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> do
        success <- c_PQgetf7 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6
        successCheck success
        (,,,,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)
          <*> (peek p4 >>= nullCheck res i 4 >>= fromSQL)
          <*> (peek p5 >>= nullCheck res i 5 >>= fromSQL)
          <*> (peek p6 >>= nullCheck res i 6 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5), pqTypesFormat (u::d6)
      , pqTypesFormat (u::d7)
      ]

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7, FromSQL b8 d8
  ) => Row (b1, b2, b3, b4, b5, b6, b7, b8)
           (d1, d2, d3, d4, d5, d6, d7, d8) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> do
        success <- c_PQgetf8 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7
        successCheck success
        (,,,,,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)
          <*> (peek p4 >>= nullCheck res i 4 >>= fromSQL)
          <*> (peek p5 >>= nullCheck res i 5 >>= fromSQL)
          <*> (peek p6 >>= nullCheck res i 6 >>= fromSQL)
          <*> (peek p7 >>= nullCheck res i 7 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5), pqTypesFormat (u::d6)
      , pqTypesFormat (u::d7), pqTypesFormat (u::d8)
      ]

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7, FromSQL b8 d8, FromSQL b9 d9
  ) => Row (b1, b2, b3, b4, b5, b6, b7, b8, b9)
           (d1, d2, d3, d4, d5, d6, d7, d8, d9) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> do
        success <- c_PQgetf9 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8
        successCheck success
        (,,,,,,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)
          <*> (peek p4 >>= nullCheck res i 4 >>= fromSQL)
          <*> (peek p5 >>= nullCheck res i 5 >>= fromSQL)
          <*> (peek p6 >>= nullCheck res i 6 >>= fromSQL)
          <*> (peek p7 >>= nullCheck res i 7 >>= fromSQL)
          <*> (peek p8 >>= nullCheck res i 8 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5), pqTypesFormat (u::d6)
      , pqTypesFormat (u::d7), pqTypesFormat (u::d8), pqTypesFormat (u::d9)
      ]

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  , FromSQL b6 d6, FromSQL b7 d7, FromSQL b8 d8, FromSQL b9 d9, FromSQL b10 d10
  ) => Row (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10)
           (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 -> do
        success <- c_PQgetf10 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9
        successCheck success
        (,,,,,,,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)
          <*> (peek p4 >>= nullCheck res i 4 >>= fromSQL)
          <*> (peek p5 >>= nullCheck res i 5 >>= fromSQL)
          <*> (peek p6 >>= nullCheck res i 6 >>= fromSQL)
          <*> (peek p7 >>= nullCheck res i 7 >>= fromSQL)
          <*> (peek p8 >>= nullCheck res i 8 >>= fromSQL)
          <*> (peek p9 >>= nullCheck res i 9 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5), pqTypesFormat (u::d6)
      , pqTypesFormat (u::d7), pqTypesFormat (u::d8), pqTypesFormat (u::d9)
      , pqTypesFormat (u::d10)
      ]

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
        success <- c_PQgetf11 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10
        successCheck success
        (,,,,,,,,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)
          <*> (peek p4 >>= nullCheck res i 4 >>= fromSQL)
          <*> (peek p5 >>= nullCheck res i 5 >>= fromSQL)
          <*> (peek p6 >>= nullCheck res i 6 >>= fromSQL)
          <*> (peek p7 >>= nullCheck res i 7 >>= fromSQL)
          <*> (peek p8 >>= nullCheck res i 8 >>= fromSQL)
          <*> (peek p9 >>= nullCheck res i 9 >>= fromSQL)
          <*> (peek p10 >>= nullCheck res i 10 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5), pqTypesFormat (u::d6)
      , pqTypesFormat (u::d7), pqTypesFormat (u::d8), pqTypesFormat (u::d9)
      , pqTypesFormat (u::d10), pqTypesFormat (u::d11)
      ]

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
        success <- c_PQgetf12 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11
        successCheck success
        (,,,,,,,,,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)
          <*> (peek p4 >>= nullCheck res i 4 >>= fromSQL)
          <*> (peek p5 >>= nullCheck res i 5 >>= fromSQL)
          <*> (peek p6 >>= nullCheck res i 6 >>= fromSQL)
          <*> (peek p7 >>= nullCheck res i 7 >>= fromSQL)
          <*> (peek p8 >>= nullCheck res i 8 >>= fromSQL)
          <*> (peek p9 >>= nullCheck res i 9 >>= fromSQL)
          <*> (peek p10 >>= nullCheck res i 10 >>= fromSQL)
          <*> (peek p11 >>= nullCheck res i 11 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5), pqTypesFormat (u::d6)
      , pqTypesFormat (u::d7), pqTypesFormat (u::d8), pqTypesFormat (u::d9)
      , pqTypesFormat (u::d10), pqTypesFormat (u::d11), pqTypesFormat (u::d12)
      ]

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
        success <- c_PQgetf13 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12
        successCheck success
        (,,,,,,,,,,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)
          <*> (peek p4 >>= nullCheck res i 4 >>= fromSQL)
          <*> (peek p5 >>= nullCheck res i 5 >>= fromSQL)
          <*> (peek p6 >>= nullCheck res i 6 >>= fromSQL)
          <*> (peek p7 >>= nullCheck res i 7 >>= fromSQL)
          <*> (peek p8 >>= nullCheck res i 8 >>= fromSQL)
          <*> (peek p9 >>= nullCheck res i 9 >>= fromSQL)
          <*> (peek p10 >>= nullCheck res i 10 >>= fromSQL)
          <*> (peek p11 >>= nullCheck res i 11 >>= fromSQL)
          <*> (peek p12 >>= nullCheck res i 12 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5), pqTypesFormat (u::d6)
      , pqTypesFormat (u::d7), pqTypesFormat (u::d8), pqTypesFormat (u::d9)
      , pqTypesFormat (u::d10), pqTypesFormat (u::d11), pqTypesFormat (u::d12)
      , pqTypesFormat (u::d13)
      ]

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
        success <- c_PQgetf14 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13
        successCheck success
        (,,,,,,,,,,,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)
          <*> (peek p4 >>= nullCheck res i 4 >>= fromSQL)
          <*> (peek p5 >>= nullCheck res i 5 >>= fromSQL)
          <*> (peek p6 >>= nullCheck res i 6 >>= fromSQL)
          <*> (peek p7 >>= nullCheck res i 7 >>= fromSQL)
          <*> (peek p8 >>= nullCheck res i 8 >>= fromSQL)
          <*> (peek p9 >>= nullCheck res i 9 >>= fromSQL)
          <*> (peek p10 >>= nullCheck res i 10 >>= fromSQL)
          <*> (peek p11 >>= nullCheck res i 11 >>= fromSQL)
          <*> (peek p12 >>= nullCheck res i 12 >>= fromSQL)
          <*> (peek p13 >>= nullCheck res i 13 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5), pqTypesFormat (u::d6)
      , pqTypesFormat (u::d7), pqTypesFormat (u::d8), pqTypesFormat (u::d9)
      , pqTypesFormat (u::d10), pqTypesFormat (u::d11), pqTypesFormat (u::d12)
      , pqTypesFormat (u::d13), pqTypesFormat (u::d14)
      ]

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
        success <- c_PQgetf15 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14
        successCheck success
        (,,,,,,,,,,,,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)
          <*> (peek p4 >>= nullCheck res i 4 >>= fromSQL)
          <*> (peek p5 >>= nullCheck res i 5 >>= fromSQL)
          <*> (peek p6 >>= nullCheck res i 6 >>= fromSQL)
          <*> (peek p7 >>= nullCheck res i 7 >>= fromSQL)
          <*> (peek p8 >>= nullCheck res i 8 >>= fromSQL)
          <*> (peek p9 >>= nullCheck res i 9 >>= fromSQL)
          <*> (peek p10 >>= nullCheck res i 10 >>= fromSQL)
          <*> (peek p11 >>= nullCheck res i 11 >>= fromSQL)
          <*> (peek p12 >>= nullCheck res i 12 >>= fromSQL)
          <*> (peek p13 >>= nullCheck res i 13 >>= fromSQL)
          <*> (peek p14 >>= nullCheck res i 14 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5), pqTypesFormat (u::d6)
      , pqTypesFormat (u::d7), pqTypesFormat (u::d8), pqTypesFormat (u::d9)
      , pqTypesFormat (u::d10), pqTypesFormat (u::d11), pqTypesFormat (u::d12)
      , pqTypesFormat (u::d13), pqTypesFormat (u::d14), pqTypesFormat (u::d15)
      ]

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
        success <- c_PQgetf16 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15
        successCheck success
        (,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)
          <*> (peek p4 >>= nullCheck res i 4 >>= fromSQL)
          <*> (peek p5 >>= nullCheck res i 5 >>= fromSQL)
          <*> (peek p6 >>= nullCheck res i 6 >>= fromSQL)
          <*> (peek p7 >>= nullCheck res i 7 >>= fromSQL)
          <*> (peek p8 >>= nullCheck res i 8 >>= fromSQL)
          <*> (peek p9 >>= nullCheck res i 9 >>= fromSQL)
          <*> (peek p10 >>= nullCheck res i 10 >>= fromSQL)
          <*> (peek p11 >>= nullCheck res i 11 >>= fromSQL)
          <*> (peek p12 >>= nullCheck res i 12 >>= fromSQL)
          <*> (peek p13 >>= nullCheck res i 13 >>= fromSQL)
          <*> (peek p14 >>= nullCheck res i 14 >>= fromSQL)
          <*> (peek p15 >>= nullCheck res i 15 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5), pqTypesFormat (u::d6)
      , pqTypesFormat (u::d7), pqTypesFormat (u::d8), pqTypesFormat (u::d9)
      , pqTypesFormat (u::d10), pqTypesFormat (u::d11), pqTypesFormat (u::d12)
      , pqTypesFormat (u::d13), pqTypesFormat (u::d14), pqTypesFormat (u::d15)
      , pqTypesFormat (u::d16)
      ]

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
        success <- c_PQgetf17 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15 16 p16
        successCheck success
        (,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)
          <*> (peek p4 >>= nullCheck res i 4 >>= fromSQL)
          <*> (peek p5 >>= nullCheck res i 5 >>= fromSQL)
          <*> (peek p6 >>= nullCheck res i 6 >>= fromSQL)
          <*> (peek p7 >>= nullCheck res i 7 >>= fromSQL)
          <*> (peek p8 >>= nullCheck res i 8 >>= fromSQL)
          <*> (peek p9 >>= nullCheck res i 9 >>= fromSQL)
          <*> (peek p10 >>= nullCheck res i 10 >>= fromSQL)
          <*> (peek p11 >>= nullCheck res i 11 >>= fromSQL)
          <*> (peek p12 >>= nullCheck res i 12 >>= fromSQL)
          <*> (peek p13 >>= nullCheck res i 13 >>= fromSQL)
          <*> (peek p14 >>= nullCheck res i 14 >>= fromSQL)
          <*> (peek p15 >>= nullCheck res i 15 >>= fromSQL)
          <*> (peek p16 >>= nullCheck res i 16 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5), pqTypesFormat (u::d6)
      , pqTypesFormat (u::d7), pqTypesFormat (u::d8), pqTypesFormat (u::d9)
      , pqTypesFormat (u::d10), pqTypesFormat (u::d11), pqTypesFormat (u::d12)
      , pqTypesFormat (u::d13), pqTypesFormat (u::d14), pqTypesFormat (u::d15)
      , pqTypesFormat (u::d16), pqTypesFormat (u::d17)
      ]

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
        success <- c_PQgetf18 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15 16 p16 17 p17
        successCheck success
        (,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)
          <*> (peek p4 >>= nullCheck res i 4 >>= fromSQL)
          <*> (peek p5 >>= nullCheck res i 5 >>= fromSQL)
          <*> (peek p6 >>= nullCheck res i 6 >>= fromSQL)
          <*> (peek p7 >>= nullCheck res i 7 >>= fromSQL)
          <*> (peek p8 >>= nullCheck res i 8 >>= fromSQL)
          <*> (peek p9 >>= nullCheck res i 9 >>= fromSQL)
          <*> (peek p10 >>= nullCheck res i 10 >>= fromSQL)
          <*> (peek p11 >>= nullCheck res i 11 >>= fromSQL)
          <*> (peek p12 >>= nullCheck res i 12 >>= fromSQL)
          <*> (peek p13 >>= nullCheck res i 13 >>= fromSQL)
          <*> (peek p14 >>= nullCheck res i 14 >>= fromSQL)
          <*> (peek p15 >>= nullCheck res i 15 >>= fromSQL)
          <*> (peek p16 >>= nullCheck res i 16 >>= fromSQL)
          <*> (peek p17 >>= nullCheck res i 17 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5), pqTypesFormat (u::d6)
      , pqTypesFormat (u::d7), pqTypesFormat (u::d8), pqTypesFormat (u::d9)
      , pqTypesFormat (u::d10), pqTypesFormat (u::d11), pqTypesFormat (u::d12)
      , pqTypesFormat (u::d13), pqTypesFormat (u::d14), pqTypesFormat (u::d15)
      , pqTypesFormat (u::d16), pqTypesFormat (u::d17), pqTypesFormat (u::d18)
      ]

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
        success <- c_PQgetf19 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15 16 p16 17 p17 18 p18
        successCheck success
        (,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)
          <*> (peek p4 >>= nullCheck res i 4 >>= fromSQL)
          <*> (peek p5 >>= nullCheck res i 5 >>= fromSQL)
          <*> (peek p6 >>= nullCheck res i 6 >>= fromSQL)
          <*> (peek p7 >>= nullCheck res i 7 >>= fromSQL)
          <*> (peek p8 >>= nullCheck res i 8 >>= fromSQL)
          <*> (peek p9 >>= nullCheck res i 9 >>= fromSQL)
          <*> (peek p10 >>= nullCheck res i 10 >>= fromSQL)
          <*> (peek p11 >>= nullCheck res i 11 >>= fromSQL)
          <*> (peek p12 >>= nullCheck res i 12 >>= fromSQL)
          <*> (peek p13 >>= nullCheck res i 13 >>= fromSQL)
          <*> (peek p14 >>= nullCheck res i 14 >>= fromSQL)
          <*> (peek p15 >>= nullCheck res i 15 >>= fromSQL)
          <*> (peek p16 >>= nullCheck res i 16 >>= fromSQL)
          <*> (peek p17 >>= nullCheck res i 17 >>= fromSQL)
          <*> (peek p18 >>= nullCheck res i 18 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5), pqTypesFormat (u::d6)
      , pqTypesFormat (u::d7), pqTypesFormat (u::d8), pqTypesFormat (u::d9)
      , pqTypesFormat (u::d10), pqTypesFormat (u::d11), pqTypesFormat (u::d12)
      , pqTypesFormat (u::d13), pqTypesFormat (u::d14), pqTypesFormat (u::d15)
      , pqTypesFormat (u::d16), pqTypesFormat (u::d17), pqTypesFormat (u::d18)
      , pqTypesFormat (u::d19)
      ]

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
        success <- c_PQgetf20 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15 16 p16 17 p17 18 p18 19 p19
        successCheck success
        (,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= nullCheck res i 0 >>= fromSQL)
          <*> (peek p1 >>= nullCheck res i 1 >>= fromSQL)
          <*> (peek p2 >>= nullCheck res i 2 >>= fromSQL)
          <*> (peek p3 >>= nullCheck res i 3 >>= fromSQL)
          <*> (peek p4 >>= nullCheck res i 4 >>= fromSQL)
          <*> (peek p5 >>= nullCheck res i 5 >>= fromSQL)
          <*> (peek p6 >>= nullCheck res i 6 >>= fromSQL)
          <*> (peek p7 >>= nullCheck res i 7 >>= fromSQL)
          <*> (peek p8 >>= nullCheck res i 8 >>= fromSQL)
          <*> (peek p9 >>= nullCheck res i 9 >>= fromSQL)
          <*> (peek p10 >>= nullCheck res i 10 >>= fromSQL)
          <*> (peek p11 >>= nullCheck res i 11 >>= fromSQL)
          <*> (peek p12 >>= nullCheck res i 12 >>= fromSQL)
          <*> (peek p13 >>= nullCheck res i 13 >>= fromSQL)
          <*> (peek p14 >>= nullCheck res i 14 >>= fromSQL)
          <*> (peek p15 >>= nullCheck res i 15 >>= fromSQL)
          <*> (peek p16 >>= nullCheck res i 16 >>= fromSQL)
          <*> (peek p17 >>= nullCheck res i 17 >>= fromSQL)
          <*> (peek p18 >>= nullCheck res i 18 >>= fromSQL)
          <*> (peek p19 >>= nullCheck res i 19 >>= fromSQL)

    rowFormat _ = BS.concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5), pqTypesFormat (u::d6)
      , pqTypesFormat (u::d7), pqTypesFormat (u::d8), pqTypesFormat (u::d9)
      , pqTypesFormat (u::d10), pqTypesFormat (u::d11), pqTypesFormat (u::d12)
      , pqTypesFormat (u::d13), pqTypesFormat (u::d14), pqTypesFormat (u::d15)
      , pqTypesFormat (u::d16), pqTypesFormat (u::d17), pqTypesFormat (u::d18)
      , pqTypesFormat (u::d19), pqTypesFormat (u::d20)
      ]
