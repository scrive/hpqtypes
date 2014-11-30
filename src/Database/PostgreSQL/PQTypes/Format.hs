{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module Database.PostgreSQL.PQTypes.Format (
    PQFormat(..)
  ) where

import Data.Int
import Data.Functor.Identity
import Data.Time
import Data.Text (Text)
import Data.Word
import qualified Data.ByteString.Char8 as BS

u :: a
u = undefined

----------------------------------------

class PQFormat t where
  -- | Map type to its libpqtypes format. Note that it should
  -- ignore its argument so that passing 'undefined' is safe.
  pqFormat    :: t -> BS.ByteString
  -- | Map type to number of type formats it contains.
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
  pqFormat _ = BS.pack "%btext"

instance PQFormat BS.ByteString where
  pqFormat _ = BS.pack "%btext"

instance PQFormat Text where
  pqFormat _ = BS.pack "%btext"

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
    PQFormat t
  ) => PQFormat (Identity t) where
    pqFormat _ = pqFormat (u::t)
    pqVariables _ = 1

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

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21)
      ]
    pqVariables _ = 21

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22)
      ]
    pqVariables _ = 22

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23)
      ]
    pqVariables _ = 23

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      ]
    pqVariables _ = 24

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25)
      ]
    pqVariables _ = 25

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26)
      ]
    pqVariables _ = 26

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27)
      ]
    pqVariables _ = 27

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      ]
    pqVariables _ = 28

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29)
      ]
    pqVariables _ = 29

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30)
      ]
    pqVariables _ = 30

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31)
      ]
    pqVariables _ = 31

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      ]
    pqVariables _ = 32

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33)
      ]
    pqVariables _ = 33

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34)
      ]
    pqVariables _ = 34

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34), pqFormat (u::t35)
      ]
    pqVariables _ = 35

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34), pqFormat (u::t35), pqFormat (u::t36)
      ]
    pqVariables _ = 36

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34), pqFormat (u::t35), pqFormat (u::t36)
      , pqFormat (u::t37)
      ]
    pqVariables _ = 37

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34), pqFormat (u::t35), pqFormat (u::t36)
      , pqFormat (u::t37), pqFormat (u::t38)
      ]
    pqVariables _ = 38

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34), pqFormat (u::t35), pqFormat (u::t36)
      , pqFormat (u::t37), pqFormat (u::t38), pqFormat (u::t39)
      ]
    pqVariables _ = 39

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34), pqFormat (u::t35), pqFormat (u::t36)
      , pqFormat (u::t37), pqFormat (u::t38), pqFormat (u::t39), pqFormat (u::t40)
      ]
    pqVariables _ = 40

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34), pqFormat (u::t35), pqFormat (u::t36)
      , pqFormat (u::t37), pqFormat (u::t38), pqFormat (u::t39), pqFormat (u::t40)
      , pqFormat (u::t41)
      ]
    pqVariables _ = 41

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34), pqFormat (u::t35), pqFormat (u::t36)
      , pqFormat (u::t37), pqFormat (u::t38), pqFormat (u::t39), pqFormat (u::t40)
      , pqFormat (u::t41), pqFormat (u::t42)
      ]
    pqVariables _ = 42

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  , PQFormat t43
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34), pqFormat (u::t35), pqFormat (u::t36)
      , pqFormat (u::t37), pqFormat (u::t38), pqFormat (u::t39), pqFormat (u::t40)
      , pqFormat (u::t41), pqFormat (u::t42), pqFormat (u::t43)
      ]
    pqVariables _ = 43

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  , PQFormat t43, PQFormat t44
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34), pqFormat (u::t35), pqFormat (u::t36)
      , pqFormat (u::t37), pqFormat (u::t38), pqFormat (u::t39), pqFormat (u::t40)
      , pqFormat (u::t41), pqFormat (u::t42), pqFormat (u::t43), pqFormat (u::t44)
      ]
    pqVariables _ = 44

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  , PQFormat t43, PQFormat t44, PQFormat t45
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34), pqFormat (u::t35), pqFormat (u::t36)
      , pqFormat (u::t37), pqFormat (u::t38), pqFormat (u::t39), pqFormat (u::t40)
      , pqFormat (u::t41), pqFormat (u::t42), pqFormat (u::t43), pqFormat (u::t44)
      , pqFormat (u::t45)
      ]
    pqVariables _ = 45

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  , PQFormat t43, PQFormat t44, PQFormat t45, PQFormat t46
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34), pqFormat (u::t35), pqFormat (u::t36)
      , pqFormat (u::t37), pqFormat (u::t38), pqFormat (u::t39), pqFormat (u::t40)
      , pqFormat (u::t41), pqFormat (u::t42), pqFormat (u::t43), pqFormat (u::t44)
      , pqFormat (u::t45), pqFormat (u::t46)
      ]
    pqVariables _ = 46

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  , PQFormat t43, PQFormat t44, PQFormat t45, PQFormat t46, PQFormat t47
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34), pqFormat (u::t35), pqFormat (u::t36)
      , pqFormat (u::t37), pqFormat (u::t38), pqFormat (u::t39), pqFormat (u::t40)
      , pqFormat (u::t41), pqFormat (u::t42), pqFormat (u::t43), pqFormat (u::t44)
      , pqFormat (u::t45), pqFormat (u::t46), pqFormat (u::t47)
      ]
    pqVariables _ = 47

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  , PQFormat t43, PQFormat t44, PQFormat t45, PQFormat t46, PQFormat t47, PQFormat t48
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34), pqFormat (u::t35), pqFormat (u::t36)
      , pqFormat (u::t37), pqFormat (u::t38), pqFormat (u::t39), pqFormat (u::t40)
      , pqFormat (u::t41), pqFormat (u::t42), pqFormat (u::t43), pqFormat (u::t44)
      , pqFormat (u::t45), pqFormat (u::t46), pqFormat (u::t47), pqFormat (u::t48)
      ]
    pqVariables _ = 48

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  , PQFormat t43, PQFormat t44, PQFormat t45, PQFormat t46, PQFormat t47, PQFormat t48
  , PQFormat t49
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34), pqFormat (u::t35), pqFormat (u::t36)
      , pqFormat (u::t37), pqFormat (u::t38), pqFormat (u::t39), pqFormat (u::t40)
      , pqFormat (u::t41), pqFormat (u::t42), pqFormat (u::t43), pqFormat (u::t44)
      , pqFormat (u::t45), pqFormat (u::t46), pqFormat (u::t47), pqFormat (u::t48)
      , pqFormat (u::t49)
      ]
    pqVariables _ = 49

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  , PQFormat t43, PQFormat t44, PQFormat t45, PQFormat t46, PQFormat t47, PQFormat t48
  , PQFormat t49, PQFormat t50
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50) where
    pqFormat _ = BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      , pqFormat (u::t33), pqFormat (u::t34), pqFormat (u::t35), pqFormat (u::t36)
      , pqFormat (u::t37), pqFormat (u::t38), pqFormat (u::t39), pqFormat (u::t40)
      , pqFormat (u::t41), pqFormat (u::t42), pqFormat (u::t43), pqFormat (u::t44)
      , pqFormat (u::t45), pqFormat (u::t46), pqFormat (u::t47), pqFormat (u::t48)
      , pqFormat (u::t49), pqFormat (u::t50)
      ]
    pqVariables _ = 50
