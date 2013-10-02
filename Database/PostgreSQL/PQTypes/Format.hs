{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module Database.PostgreSQL.PQTypes.Format (
    PQFormat(..)
  ) where

import Data.Int
import Data.Time
import Data.Text (Text)
import Data.Word
import qualified Data.ByteString.Char8 as BS

u :: a
u = undefined

----------------------------------------

class PQFormat t where
  pqFormat    :: t -> BS.ByteString
  pqVariables :: t -> Int
  pqVariables _ = 1

-- NULLables

instance PQFormat t => PQFormat (Maybe t) where
  pqFormat _ = pqFormat (u::t)
  pqVariables _ = pqVariables (u::t)

-- NUMERICS

instance PQFormat Int16 where
  pqFormat _ = BS.pack "%int2"

instance PQFormat Int32 where
  pqFormat _ = BS.pack "%int4"

instance PQFormat Int64 where
  pqFormat _ = BS.pack "%int8"

instance PQFormat Float where
  pqFormat _ = BS.pack "%float4"

instance PQFormat Double where
  pqFormat _ = BS.pack "%float8"

-- CHAR

instance PQFormat Char where
  pqFormat _ = BS.pack "%char"

instance PQFormat Word8 where
  pqFormat _ = BS.pack "%char"

-- VARIABLE-LENGTH CHARACTER TYPES

instance PQFormat String where
  pqFormat _ = BS.pack "%text"

instance PQFormat BS.ByteString where
  pqFormat _ = BS.pack "%text"

instance PQFormat Text where
  pqFormat _ = BS.pack "%text"

-- DATE

instance PQFormat Day where
  pqFormat _ = BS.pack "%date"

-- TIME

instance PQFormat TimeOfDay where
  pqFormat _ = BS.pack "%time"

-- TIMESTAMP

instance PQFormat LocalTime where
  pqFormat _ = BS.pack "%timestamp"

-- TIMESTAMPTZ

instance PQFormat UTCTime where
  pqFormat _ = BS.pack "%timestamptz"

instance PQFormat ZonedTime where
  pqFormat _ = BS.pack "%timestamptz"

-- BOOL

instance PQFormat Bool where
  pqFormat _ = BS.pack "%bool"

-- TUPLES

instance PQFormat () where
    pqFormat _ = BS.empty
    pqVariables _ = 0

instance (
    PQFormat t1, PQFormat t2
  ) => PQFormat (t1, t2) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2)
      ]
    pqVariables _ = 2

instance (
    PQFormat t1, PQFormat t2, PQFormat t3
  ) => PQFormat (t1, t2, t3) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3)
      ]
    pqVariables _ = 3

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4
  ) => PQFormat (t1, t2, t3, t4) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      ]
    pqVariables _ = 4

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5
  ) => PQFormat (t1, t2, t3, t4, t5) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5)
      ]
    pqVariables _ = 5

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  ) => PQFormat (t1, t2, t3, t4, t5, t6) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6)
      ]
    pqVariables _ = 6

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7)
      ]
    pqVariables _ = 7

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      ]
    pqVariables _ = 8

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9)
      ]
    pqVariables _ = 9

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10)
      ]
    pqVariables _ = 10

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11)
      ]
    pqVariables _ = 11

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      ]
    pqVariables _ = 12

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13)
      ]
    pqVariables _ = 13

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14)
      ]
    pqVariables _ = 14

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15)
      ]
    pqVariables _ = 15

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      ]
    pqVariables _ = 16

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17)
      ]
    pqVariables _ = 17

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18)
      ]
    pqVariables _ = 18

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19)
      ]
    pqVariables _ = 19

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      ]
    pqVariables _ = 20
