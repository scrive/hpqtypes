{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, OverlappingInstances
  , ScopedTypeVariables, UndecidableInstances #-}
module DB.Row (Row(..)) where

import Control.Applicative
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr

import DB.FromSQL
import DB.Primitive.Get
import DB.Primitive.Types
import DB.Primitive.Interface

u :: a
u = undefined

isColumnNull :: Ptr PGresult -> CInt -> CInt -> Bool
isColumnNull res tuple column = pqGetIsNull res tuple column == 1

checkSuccess :: CInt -> IO ()
checkSuccess 0 = pqGetError >>= peekCString >>= error
checkSuccess _ = return ()

----------------------------------------

class Row base dest | dest -> base where
  rowFormat :: dest -> String
  parseRow :: Ptr PGresult -> CInt -> CString -> IO dest

instance FromSQL base dest => Row base dest where
  parseRow res i fmt = alloca $ \ptr -> do
    success <- pqGet1 res i fmt 0 ptr
    checkSuccess success
    peek ptr >>= fromSQL (isColumnNull res i 0)

  rowFormat = pqTypesFormat

instance (
    FromSQL b1 d1, FromSQL b2 d2
  ) => Row (b1, b2)
           (d1, d2) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> do
        success <- pqGet2 res i fmt 0 p0 1 p1
        checkSuccess success
        (,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))

    rowFormat _ = concat [pqTypesFormat (u::d1), pqTypesFormat (u::d2)]

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3
  ) => Row (b1, b2, b3)
           (d1, d2, d3) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> do
        success <- pqGet3 res i fmt 0 p0 1 p1 2 p2
        checkSuccess success
        (,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))

    rowFormat _ = concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      ]

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4
  ) => Row (b1, b2, b3, b4)
           (d1, d2, d3, d4) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> do
        success <- pqGet4 res i fmt 0 p0 1 p1 2 p2 3 p3
        checkSuccess success
        (,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))

    rowFormat _ = concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4)
      ]

instance (
    FromSQL b1 d1, FromSQL b2 d2, FromSQL b3 d3, FromSQL b4 d4, FromSQL b5 d5
  ) => Row (b1, b2, b3, b4, b5)
           (d1, d2, d3, d4, d5) where
    parseRow res i fmt =
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 -> do
        success <- pqGet5 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4
        checkSuccess success
        (,,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))
          <*> (peek p4 >>= fromSQL (isColumnNull res i 4))

    rowFormat _ = concat [
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
        success <- pqGet6 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5
        checkSuccess success
        (,,,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))
          <*> (peek p4 >>= fromSQL (isColumnNull res i 4))
          <*> (peek p5 >>= fromSQL (isColumnNull res i 5))

    rowFormat _ = concat [
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
        success <- pqGet7 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6
        checkSuccess success
        (,,,,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))
          <*> (peek p4 >>= fromSQL (isColumnNull res i 4))
          <*> (peek p5 >>= fromSQL (isColumnNull res i 5))
          <*> (peek p6 >>= fromSQL (isColumnNull res i 6))

    rowFormat _ = concat [
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
        success <- pqGet8 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7
        checkSuccess success
        (,,,,,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))
          <*> (peek p4 >>= fromSQL (isColumnNull res i 4))
          <*> (peek p5 >>= fromSQL (isColumnNull res i 5))
          <*> (peek p6 >>= fromSQL (isColumnNull res i 6))
          <*> (peek p7 >>= fromSQL (isColumnNull res i 7))

    rowFormat _ = concat [
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
        success <- pqGet9 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8
        checkSuccess success
        (,,,,,,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))
          <*> (peek p4 >>= fromSQL (isColumnNull res i 4))
          <*> (peek p5 >>= fromSQL (isColumnNull res i 5))
          <*> (peek p6 >>= fromSQL (isColumnNull res i 6))
          <*> (peek p7 >>= fromSQL (isColumnNull res i 7))
          <*> (peek p8 >>= fromSQL (isColumnNull res i 8))

    rowFormat _ = concat [
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
        success <- pqGet10 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9
        checkSuccess success
        (,,,,,,,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))
          <*> (peek p4 >>= fromSQL (isColumnNull res i 4))
          <*> (peek p5 >>= fromSQL (isColumnNull res i 5))
          <*> (peek p6 >>= fromSQL (isColumnNull res i 6))
          <*> (peek p7 >>= fromSQL (isColumnNull res i 7))
          <*> (peek p8 >>= fromSQL (isColumnNull res i 8))
          <*> (peek p9 >>= fromSQL (isColumnNull res i 9))

    rowFormat _ = concat [
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
        success <- pqGet11 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10
        checkSuccess success
        (,,,,,,,,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))
          <*> (peek p4 >>= fromSQL (isColumnNull res i 4))
          <*> (peek p5 >>= fromSQL (isColumnNull res i 5))
          <*> (peek p6 >>= fromSQL (isColumnNull res i 6))
          <*> (peek p7 >>= fromSQL (isColumnNull res i 7))
          <*> (peek p8 >>= fromSQL (isColumnNull res i 8))
          <*> (peek p9 >>= fromSQL (isColumnNull res i 9))
          <*> (peek p10 >>= fromSQL (isColumnNull res i 10))

    rowFormat _ = concat [
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
        success <- pqGet12 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11
        checkSuccess success
        (,,,,,,,,,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))
          <*> (peek p4 >>= fromSQL (isColumnNull res i 4))
          <*> (peek p5 >>= fromSQL (isColumnNull res i 5))
          <*> (peek p6 >>= fromSQL (isColumnNull res i 6))
          <*> (peek p7 >>= fromSQL (isColumnNull res i 7))
          <*> (peek p8 >>= fromSQL (isColumnNull res i 8))
          <*> (peek p9 >>= fromSQL (isColumnNull res i 9))
          <*> (peek p10 >>= fromSQL (isColumnNull res i 10))
          <*> (peek p11 >>= fromSQL (isColumnNull res i 11))

    rowFormat _ = concat [
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
        success <- pqGet13 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12
        checkSuccess success
        (,,,,,,,,,,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))
          <*> (peek p4 >>= fromSQL (isColumnNull res i 4))
          <*> (peek p5 >>= fromSQL (isColumnNull res i 5))
          <*> (peek p6 >>= fromSQL (isColumnNull res i 6))
          <*> (peek p7 >>= fromSQL (isColumnNull res i 7))
          <*> (peek p8 >>= fromSQL (isColumnNull res i 8))
          <*> (peek p9 >>= fromSQL (isColumnNull res i 9))
          <*> (peek p10 >>= fromSQL (isColumnNull res i 10))
          <*> (peek p11 >>= fromSQL (isColumnNull res i 11))
          <*> (peek p12 >>= fromSQL (isColumnNull res i 12))

    rowFormat _ = concat [
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
        success <- pqGet14 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13
        checkSuccess success
        (,,,,,,,,,,,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))
          <*> (peek p4 >>= fromSQL (isColumnNull res i 4))
          <*> (peek p5 >>= fromSQL (isColumnNull res i 5))
          <*> (peek p6 >>= fromSQL (isColumnNull res i 6))
          <*> (peek p7 >>= fromSQL (isColumnNull res i 7))
          <*> (peek p8 >>= fromSQL (isColumnNull res i 8))
          <*> (peek p9 >>= fromSQL (isColumnNull res i 9))
          <*> (peek p10 >>= fromSQL (isColumnNull res i 10))
          <*> (peek p11 >>= fromSQL (isColumnNull res i 11))
          <*> (peek p12 >>= fromSQL (isColumnNull res i 12))
          <*> (peek p13 >>= fromSQL (isColumnNull res i 13))

    rowFormat _ = concat [
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
        success <- pqGet15 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14
        checkSuccess success
        (,,,,,,,,,,,,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))
          <*> (peek p4 >>= fromSQL (isColumnNull res i 4))
          <*> (peek p5 >>= fromSQL (isColumnNull res i 5))
          <*> (peek p6 >>= fromSQL (isColumnNull res i 6))
          <*> (peek p7 >>= fromSQL (isColumnNull res i 7))
          <*> (peek p8 >>= fromSQL (isColumnNull res i 8))
          <*> (peek p9 >>= fromSQL (isColumnNull res i 9))
          <*> (peek p10 >>= fromSQL (isColumnNull res i 10))
          <*> (peek p11 >>= fromSQL (isColumnNull res i 11))
          <*> (peek p12 >>= fromSQL (isColumnNull res i 12))
          <*> (peek p13 >>= fromSQL (isColumnNull res i 13))
          <*> (peek p14 >>= fromSQL (isColumnNull res i 14))

    rowFormat _ = concat [
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
        success <- pqGet16 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15
        checkSuccess success
        (,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))
          <*> (peek p4 >>= fromSQL (isColumnNull res i 4))
          <*> (peek p5 >>= fromSQL (isColumnNull res i 5))
          <*> (peek p6 >>= fromSQL (isColumnNull res i 6))
          <*> (peek p7 >>= fromSQL (isColumnNull res i 7))
          <*> (peek p8 >>= fromSQL (isColumnNull res i 8))
          <*> (peek p9 >>= fromSQL (isColumnNull res i 9))
          <*> (peek p10 >>= fromSQL (isColumnNull res i 10))
          <*> (peek p11 >>= fromSQL (isColumnNull res i 11))
          <*> (peek p12 >>= fromSQL (isColumnNull res i 12))
          <*> (peek p13 >>= fromSQL (isColumnNull res i 13))
          <*> (peek p14 >>= fromSQL (isColumnNull res i 14))
          <*> (peek p15 >>= fromSQL (isColumnNull res i 15))

    rowFormat _ = concat [
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
        success <- pqGet17 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15 16 p16
        checkSuccess success
        (,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))
          <*> (peek p4 >>= fromSQL (isColumnNull res i 4))
          <*> (peek p5 >>= fromSQL (isColumnNull res i 5))
          <*> (peek p6 >>= fromSQL (isColumnNull res i 6))
          <*> (peek p7 >>= fromSQL (isColumnNull res i 7))
          <*> (peek p8 >>= fromSQL (isColumnNull res i 8))
          <*> (peek p9 >>= fromSQL (isColumnNull res i 9))
          <*> (peek p10 >>= fromSQL (isColumnNull res i 10))
          <*> (peek p11 >>= fromSQL (isColumnNull res i 11))
          <*> (peek p12 >>= fromSQL (isColumnNull res i 12))
          <*> (peek p13 >>= fromSQL (isColumnNull res i 13))
          <*> (peek p14 >>= fromSQL (isColumnNull res i 14))
          <*> (peek p15 >>= fromSQL (isColumnNull res i 15))
          <*> (peek p16 >>= fromSQL (isColumnNull res i 16))

    rowFormat _ = concat [
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
        success <- pqGet18 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15 16 p16 17 p17
        checkSuccess success
        (,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))
          <*> (peek p4 >>= fromSQL (isColumnNull res i 4))
          <*> (peek p5 >>= fromSQL (isColumnNull res i 5))
          <*> (peek p6 >>= fromSQL (isColumnNull res i 6))
          <*> (peek p7 >>= fromSQL (isColumnNull res i 7))
          <*> (peek p8 >>= fromSQL (isColumnNull res i 8))
          <*> (peek p9 >>= fromSQL (isColumnNull res i 9))
          <*> (peek p10 >>= fromSQL (isColumnNull res i 10))
          <*> (peek p11 >>= fromSQL (isColumnNull res i 11))
          <*> (peek p12 >>= fromSQL (isColumnNull res i 12))
          <*> (peek p13 >>= fromSQL (isColumnNull res i 13))
          <*> (peek p14 >>= fromSQL (isColumnNull res i 14))
          <*> (peek p15 >>= fromSQL (isColumnNull res i 15))
          <*> (peek p16 >>= fromSQL (isColumnNull res i 16))
          <*> (peek p17 >>= fromSQL (isColumnNull res i 17))

    rowFormat _ = concat [
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
        success <- pqGet19 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15 16 p16 17 p17 18 p18
        checkSuccess success
        (,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))
          <*> (peek p4 >>= fromSQL (isColumnNull res i 4))
          <*> (peek p5 >>= fromSQL (isColumnNull res i 5))
          <*> (peek p6 >>= fromSQL (isColumnNull res i 6))
          <*> (peek p7 >>= fromSQL (isColumnNull res i 7))
          <*> (peek p8 >>= fromSQL (isColumnNull res i 8))
          <*> (peek p9 >>= fromSQL (isColumnNull res i 9))
          <*> (peek p10 >>= fromSQL (isColumnNull res i 10))
          <*> (peek p11 >>= fromSQL (isColumnNull res i 11))
          <*> (peek p12 >>= fromSQL (isColumnNull res i 12))
          <*> (peek p13 >>= fromSQL (isColumnNull res i 13))
          <*> (peek p14 >>= fromSQL (isColumnNull res i 14))
          <*> (peek p15 >>= fromSQL (isColumnNull res i 15))
          <*> (peek p16 >>= fromSQL (isColumnNull res i 16))
          <*> (peek p17 >>= fromSQL (isColumnNull res i 17))
          <*> (peek p18 >>= fromSQL (isColumnNull res i 18))

    rowFormat _ = concat [
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
        success <- pqGet20 res i fmt 0 p0 1 p1 2 p2 3 p3 4 p4 5 p5 6 p6 7 p7 8 p8 9 p9 10 p10 11 p11 12 p12 13 p13 14 p14 15 p15 16 p16 17 p17 18 p18 19 p19
        checkSuccess success
        (,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= fromSQL (isColumnNull res i 0))
          <*> (peek p1 >>= fromSQL (isColumnNull res i 1))
          <*> (peek p2 >>= fromSQL (isColumnNull res i 2))
          <*> (peek p3 >>= fromSQL (isColumnNull res i 3))
          <*> (peek p4 >>= fromSQL (isColumnNull res i 4))
          <*> (peek p5 >>= fromSQL (isColumnNull res i 5))
          <*> (peek p6 >>= fromSQL (isColumnNull res i 6))
          <*> (peek p7 >>= fromSQL (isColumnNull res i 7))
          <*> (peek p8 >>= fromSQL (isColumnNull res i 8))
          <*> (peek p9 >>= fromSQL (isColumnNull res i 9))
          <*> (peek p10 >>= fromSQL (isColumnNull res i 10))
          <*> (peek p11 >>= fromSQL (isColumnNull res i 11))
          <*> (peek p12 >>= fromSQL (isColumnNull res i 12))
          <*> (peek p13 >>= fromSQL (isColumnNull res i 13))
          <*> (peek p14 >>= fromSQL (isColumnNull res i 14))
          <*> (peek p15 >>= fromSQL (isColumnNull res i 15))
          <*> (peek p16 >>= fromSQL (isColumnNull res i 16))
          <*> (peek p17 >>= fromSQL (isColumnNull res i 17))
          <*> (peek p18 >>= fromSQL (isColumnNull res i 18))
          <*> (peek p19 >>= fromSQL (isColumnNull res i 19))

    rowFormat _ = concat [
        pqTypesFormat (u::d1), pqTypesFormat (u::d2), pqTypesFormat (u::d3)
      , pqTypesFormat (u::d4), pqTypesFormat (u::d5), pqTypesFormat (u::d6)
      , pqTypesFormat (u::d7), pqTypesFormat (u::d8), pqTypesFormat (u::d9)
      , pqTypesFormat (u::d10), pqTypesFormat (u::d11), pqTypesFormat (u::d12)
      , pqTypesFormat (u::d13), pqTypesFormat (u::d14), pqTypesFormat (u::d15)
      , pqTypesFormat (u::d16), pqTypesFormat (u::d17), pqTypesFormat (u::d18)
      , pqTypesFormat (u::d19), pqTypesFormat (u::d20)
      ]
