module Database.PostgreSQL.PQTypes.Format (
    PQFormat(..)
  , (:*:)(..)
  ) where

import Data.Int
import Data.Functor.Identity
import Data.Time
import Data.Typeable
import Data.Word
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

u :: a
u = undefined

----------------------------------------

class PQFormat t where
  -- | Map type to its libpqtypes format. Note that it should
  -- ignore its argument so that passing 'undefined' is safe.
  pqFormat :: t -> BS.ByteString

  -- | Map type to its null-terminated libpqtypes format, so
  -- it can safely be used by 'unsafeUseAsCString'. Also, for
  -- a specific type it becomes a top level CAF, therefore it
  -- will be computed by GHC at most once.
  pqFormat0 :: t -> BS.ByteString
  pqFormat0 = const $ pqFormat (u::t) `BS.snoc` '\0'

  -- | Map type to number of type formats it contains.
  pqVariables :: t -> Int
  pqVariables = const 1

-- CARTESIAN PRODUCT

-- | Cartesian product of rows.
data a :*: b = a :*: b
  deriving (Eq, Ord, Show, Typeable)

instance (PQFormat t1, PQFormat t2) => PQFormat (t1 :*: t2) where
  pqFormat = const $ pqFormat (u::t1) `BS.append` pqFormat (u::t2)
  pqVariables = const $ pqVariables (u::t1) + pqVariables (u::t2)

-- NULLables

instance PQFormat t => PQFormat (Maybe t) where
  pqFormat = const $ pqFormat (u::t)
  pqVariables = const $ pqVariables (u::t)

-- NUMERICS

instance PQFormat Int16 where
  pqFormat = const $ BS.pack "%int2"

instance PQFormat Int32 where
  pqFormat = const $ BS.pack "%int4"

instance PQFormat Int64 where
  pqFormat = const $ BS.pack "%int8"

instance PQFormat Int where
  pqFormat = const $ BS.pack "%int8"

instance PQFormat Float where
  pqFormat = const $ BS.pack "%float4"

instance PQFormat Double where
  pqFormat = const $ BS.pack "%float8"

-- CHAR

instance PQFormat Char where
  pqFormat = const $ BS.pack "%char"

instance PQFormat Word8 where
  pqFormat = const $ BS.pack "%char"

-- VARIABLE-LENGTH CHARACTER TYPES

instance PQFormat String where
  pqFormat = const $ BS.pack "%btext"

instance PQFormat BS.ByteString where
  pqFormat = const $ BS.pack "%btext"

instance PQFormat BSL.ByteString where
  pqFormat = const $ BS.pack "%btext"

instance PQFormat T.Text where
  pqFormat = const $ BS.pack "%btext"

instance PQFormat TL.Text where
  pqFormat = const $ BS.pack "%btext"

-- DATE

instance PQFormat Day where
  pqFormat = const $ BS.pack "%date"

-- TIME

instance PQFormat TimeOfDay where
  pqFormat = const $ BS.pack "%time"

-- TIMESTAMP

instance PQFormat LocalTime where
  pqFormat = const $ BS.pack "%timestamp"

-- TIMESTAMPTZ

instance PQFormat UTCTime where
  pqFormat = const $ BS.pack "%timestamptz"

instance PQFormat ZonedTime where
  pqFormat = const $ BS.pack "%timestamptz"

-- BOOL

instance PQFormat Bool where
  pqFormat = const $ BS.pack "%bool"

-- TUPLES

instance PQFormat () where
  pqFormat = const $ BS.empty
  pqVariables = const 0

instance (
    PQFormat t
  ) => PQFormat (Identity t) where
    pqFormat = const $ pqFormat (u::t)
    pqVariables = const 1

instance (
    PQFormat t1, PQFormat t2
  ) => PQFormat (t1, t2) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2)
      ]
    pqVariables = const 2

instance (
    PQFormat t1, PQFormat t2, PQFormat t3
  ) => PQFormat (t1, t2, t3) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3)
      ]
    pqVariables = const 3

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4
  ) => PQFormat (t1, t2, t3, t4) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      ]
    pqVariables = const 4

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5
  ) => PQFormat (t1, t2, t3, t4, t5) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5)
      ]
    pqVariables = const 5

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  ) => PQFormat (t1, t2, t3, t4, t5, t6) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6)
      ]
    pqVariables = const 6

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7)
      ]
    pqVariables = const 7

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      ]
    pqVariables = const 8

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9)
      ]
    pqVariables = const 9

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10)
      ]
    pqVariables = const 10

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11)
      ]
    pqVariables = const 11

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      ]
    pqVariables = const 12

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13)
      ]
    pqVariables = const 13

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14)
      ]
    pqVariables = const 14

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15)
      ]
    pqVariables = const 15

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      ]
    pqVariables = const 16

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17)
      ]
    pqVariables = const 17

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18)
      ]
    pqVariables = const 18

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19)
      ]
    pqVariables = const 19

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      ]
    pqVariables = const 20

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21)
      ]
    pqVariables = const 21

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22)
      ]
    pqVariables = const 22

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23)
      ]
    pqVariables = const 23

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      ]
    pqVariables = const 24

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25)
      ]
    pqVariables = const 25

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26)
      ]
    pqVariables = const 26

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27)
      ]
    pqVariables = const 27

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      ]
    pqVariables = const 28

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29)
      ]
    pqVariables = const 29

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30)
      ]
    pqVariables = const 30

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31)
      ]
    pqVariables = const 31

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32) where
    pqFormat = const $ BS.concat [
        pqFormat (u::t1), pqFormat (u::t2), pqFormat (u::t3), pqFormat (u::t4)
      , pqFormat (u::t5), pqFormat (u::t6), pqFormat (u::t7), pqFormat (u::t8)
      , pqFormat (u::t9), pqFormat (u::t10), pqFormat (u::t11), pqFormat (u::t12)
      , pqFormat (u::t13), pqFormat (u::t14), pqFormat (u::t15), pqFormat (u::t16)
      , pqFormat (u::t17), pqFormat (u::t18), pqFormat (u::t19), pqFormat (u::t20)
      , pqFormat (u::t21), pqFormat (u::t22), pqFormat (u::t23), pqFormat (u::t24)
      , pqFormat (u::t25), pqFormat (u::t26), pqFormat (u::t27), pqFormat (u::t28)
      , pqFormat (u::t29), pqFormat (u::t30), pqFormat (u::t31), pqFormat (u::t32)
      ]
    pqVariables = const 32

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33) where
    pqFormat = const $ BS.concat [
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
    pqVariables = const 33

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34) where
    pqFormat = const $ BS.concat [
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
    pqVariables = const 34

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35) where
    pqFormat = const $ BS.concat [
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
    pqVariables = const 35

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36) where
    pqFormat = const $ BS.concat [
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
    pqVariables = const 36

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37) where
    pqFormat = const $ BS.concat [
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
    pqVariables = const 37

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38) where
    pqFormat = const $ BS.concat [
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
    pqVariables = const 38

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39) where
    pqFormat = const $ BS.concat [
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
    pqVariables = const 39

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40) where
    pqFormat = const $ BS.concat [
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
    pqVariables = const 40

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41) where
    pqFormat = const $ BS.concat [
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
    pqVariables = const 41

instance (
    PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42) where
    pqFormat = const $ BS.concat [
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
    pqVariables = const 42

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
    pqFormat = const $ BS.concat [
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
    pqVariables = const 43

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
    pqFormat = const $ BS.concat [
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
    pqVariables = const 44

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
    pqFormat = const $ BS.concat [
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
    pqVariables = const 45

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
    pqFormat = const $ BS.concat [
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
    pqVariables = const 46

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
    pqFormat = const $ BS.concat [
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
    pqVariables = const 47

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
    pqFormat = const $ BS.concat [
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
    pqVariables = const 48

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
    pqFormat = const $ BS.concat [
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
    pqVariables = const 49

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
    pqFormat = const $ BS.concat [
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
    pqVariables = const 50
