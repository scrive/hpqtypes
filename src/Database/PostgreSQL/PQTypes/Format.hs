{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.PostgreSQL.PQTypes.Format
  ( PQFormat (..)
  , pqFormatP
  , pqFormat0P
  , pqVariablesP
  , (:*:) (..)
  ) where

import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Functor.Identity
import Data.Int
import Data.Proxy
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.UUID.Types
import Data.Word

----------------------------------------

-- | Methods in this class are supposed to be used with the
-- @TypeApplications@ extension.
class PQFormat t where
  -- | Map a type to its libpqtypes format.
  pqFormat :: BS.ByteString

  -- | Map type to its null-terminated libpqtypes format, so
  -- it can safely be used by 'unsafeUseAsCString'. Also, for
  -- a specific type it becomes a top level CAF, therefore it
  -- will be computed by GHC at most once.
  pqFormat0 :: BS.ByteString
  pqFormat0 = pqFormat @t `BS.snoc` '\0'

  -- | Map type to number of type formats it contains.
  pqVariables :: Int
  pqVariables = 1

-- Helpers that are parametrised by a 'Proxy t' instead of 't'.

pqFormatP :: forall t. PQFormat t => Proxy t -> BS.ByteString
pqFormatP _ = pqFormat @t

pqFormat0P :: forall t. PQFormat t => Proxy t -> BS.ByteString
pqFormat0P _ = pqFormat0 @t

pqVariablesP :: forall t. PQFormat t => Proxy t -> Int
pqVariablesP _ = pqVariables @t

-- CARTESIAN PRODUCT

-- | Cartesian product of rows.
data a :*: b = a :*: b
  deriving (Eq, Ord, Show)

instance (PQFormat t1, PQFormat t2) => PQFormat (t1 :*: t2) where
  pqFormat = pqFormat @t1 `BS.append` pqFormat @t2
  pqVariables = pqVariables @t1 + pqVariables @t2

-- NULLables

instance PQFormat t => PQFormat (Maybe t) where
  pqFormat = pqFormat @t
  pqVariables = pqVariables @t

-- NUMERICS

instance PQFormat Int16 where
  pqFormat = BS.pack "%int2"

instance PQFormat Int32 where
  pqFormat = BS.pack "%int4"

instance PQFormat Int64 where
  pqFormat = BS.pack "%int8"

instance PQFormat Int where
  pqFormat = BS.pack "%int8"

instance PQFormat Float where
  pqFormat = BS.pack "%float4"

instance PQFormat Double where
  pqFormat = BS.pack "%float8"

instance PQFormat Word16 where
  pqFormat = BS.pack "%int2"

instance PQFormat Word32 where
  pqFormat = BS.pack "%int4"

instance PQFormat Word64 where
  pqFormat = BS.pack "%int8"

-- CHAR

instance PQFormat Char where
  pqFormat = BS.pack "%char"

instance PQFormat Word8 where
  pqFormat = BS.pack "%char"

-- VARIABLE-LENGTH CHARACTER TYPES

instance PQFormat String where
  pqFormat = BS.pack "%btext"

instance PQFormat T.Text where
  pqFormat = BS.pack "%btext"

instance PQFormat TL.Text where
  pqFormat = BS.pack "%btext"

instance PQFormat UUID where
  pqFormat = BS.pack "%uuid"

-- BYTEA

instance PQFormat BS.ByteString where
  pqFormat = BS.pack "%bytea"

instance PQFormat BSL.ByteString where
  pqFormat = BS.pack "%bytea"

-- DATE

instance PQFormat Day where
  pqFormat = BS.pack "%date"

-- TIME

instance PQFormat TimeOfDay where
  pqFormat = BS.pack "%time"

-- TIMESTAMP

instance PQFormat LocalTime where
  pqFormat = BS.pack "%timestamp"

-- TIMESTAMPTZ

instance PQFormat UTCTime where
  pqFormat = BS.pack "%timestamptz"

instance PQFormat ZonedTime where
  pqFormat = BS.pack "%timestamptz"

-- BOOL

instance PQFormat Bool where
  pqFormat = BS.pack "%bool"

-- TUPLES

{- FOURMOLU_DISABLE -}

instance PQFormat () where
  pqFormat = BS.empty
  pqVariables = 0

instance
  ( PQFormat t
  ) => PQFormat (Identity t) where
    pqFormat = pqFormat @t
    pqVariables = 1

instance
  ( PQFormat t1, PQFormat t2
  ) => PQFormat (t1, t2) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2
      ]
    pqVariables = 2

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3
  ) => PQFormat (t1, t2, t3) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3
      ]
    pqVariables = 3

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4
  ) => PQFormat (t1, t2, t3, t4) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      ]
    pqVariables = 4

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5
  ) => PQFormat (t1, t2, t3, t4, t5) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5
      ]
    pqVariables = 5

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  ) => PQFormat (t1, t2, t3, t4, t5, t6) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6
      ]
    pqVariables = 6

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7
      ]
    pqVariables = 7

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      ]
    pqVariables = 8

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9
      ]
    pqVariables = 9

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10
      ]
    pqVariables = 10

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11
      ]
    pqVariables = 11

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      ]
    pqVariables = 12

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13
      ]
    pqVariables = 13

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14
      ]
    pqVariables = 14

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15
      ]
    pqVariables = 15

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      ]
    pqVariables = 16

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17
      ]
    pqVariables = 17

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18
      ]
    pqVariables = 18

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19
      ]
    pqVariables = 19

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      ]
    pqVariables = 20

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21
      ]
    pqVariables = 21

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22
      ]
    pqVariables = 22

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23
      ]
    pqVariables = 23

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      ]
    pqVariables = 24

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25
      ]
    pqVariables = 25

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26
      ]
    pqVariables = 26

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27
      ]
    pqVariables = 27

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      ]
    pqVariables = 28

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29
      ]
    pqVariables = 29

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30
      ]
    pqVariables = 30

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31
      ]
    pqVariables = 31

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      ]
    pqVariables = 32

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33
      ]
    pqVariables = 33

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34
      ]
    pqVariables = 34

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34, pqFormat @t35
      ]
    pqVariables = 35

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34, pqFormat @t35, pqFormat @t36
      ]
    pqVariables = 36

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34, pqFormat @t35, pqFormat @t36
      , pqFormat @t37
      ]
    pqVariables = 37

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34, pqFormat @t35, pqFormat @t36
      , pqFormat @t37, pqFormat @t38
      ]
    pqVariables = 38

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34, pqFormat @t35, pqFormat @t36
      , pqFormat @t37, pqFormat @t38, pqFormat @t39
      ]
    pqVariables = 39

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34, pqFormat @t35, pqFormat @t36
      , pqFormat @t37, pqFormat @t38, pqFormat @t39, pqFormat @t40
      ]
    pqVariables = 40

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34, pqFormat @t35, pqFormat @t36
      , pqFormat @t37, pqFormat @t38, pqFormat @t39, pqFormat @t40
      , pqFormat @t41
      ]
    pqVariables = 41

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34, pqFormat @t35, pqFormat @t36
      , pqFormat @t37, pqFormat @t38, pqFormat @t39, pqFormat @t40
      , pqFormat @t41, pqFormat @t42
      ]
    pqVariables = 42

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  , PQFormat t43
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34, pqFormat @t35, pqFormat @t36
      , pqFormat @t37, pqFormat @t38, pqFormat @t39, pqFormat @t40
      , pqFormat @t41, pqFormat @t42, pqFormat @t43
      ]
    pqVariables = 43

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  , PQFormat t43, PQFormat t44
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34, pqFormat @t35, pqFormat @t36
      , pqFormat @t37, pqFormat @t38, pqFormat @t39, pqFormat @t40
      , pqFormat @t41, pqFormat @t42, pqFormat @t43, pqFormat @t44
      ]
    pqVariables = 44

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  , PQFormat t43, PQFormat t44, PQFormat t45
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34, pqFormat @t35, pqFormat @t36
      , pqFormat @t37, pqFormat @t38, pqFormat @t39, pqFormat @t40
      , pqFormat @t41, pqFormat @t42, pqFormat @t43, pqFormat @t44
      , pqFormat @t45
      ]
    pqVariables = 45

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  , PQFormat t43, PQFormat t44, PQFormat t45, PQFormat t46
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34, pqFormat @t35, pqFormat @t36
      , pqFormat @t37, pqFormat @t38, pqFormat @t39, pqFormat @t40
      , pqFormat @t41, pqFormat @t42, pqFormat @t43, pqFormat @t44
      , pqFormat @t45, pqFormat @t46
      ]
    pqVariables = 46

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  , PQFormat t43, PQFormat t44, PQFormat t45, PQFormat t46, PQFormat t47
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34, pqFormat @t35, pqFormat @t36
      , pqFormat @t37, pqFormat @t38, pqFormat @t39, pqFormat @t40
      , pqFormat @t41, pqFormat @t42, pqFormat @t43, pqFormat @t44
      , pqFormat @t45, pqFormat @t46, pqFormat @t47
      ]
    pqVariables = 47

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  , PQFormat t43, PQFormat t44, PQFormat t45, PQFormat t46, PQFormat t47, PQFormat t48
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34, pqFormat @t35, pqFormat @t36
      , pqFormat @t37, pqFormat @t38, pqFormat @t39, pqFormat @t40
      , pqFormat @t41, pqFormat @t42, pqFormat @t43, pqFormat @t44
      , pqFormat @t45, pqFormat @t46, pqFormat @t47, pqFormat @t48
      ]
    pqVariables = 48

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  , PQFormat t43, PQFormat t44, PQFormat t45, PQFormat t46, PQFormat t47, PQFormat t48
  , PQFormat t49
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34, pqFormat @t35, pqFormat @t36
      , pqFormat @t37, pqFormat @t38, pqFormat @t39, pqFormat @t40
      , pqFormat @t41, pqFormat @t42, pqFormat @t43, pqFormat @t44
      , pqFormat @t45, pqFormat @t46, pqFormat @t47, pqFormat @t48
      , pqFormat @t49
      ]
    pqVariables = 49

instance
  ( PQFormat t1, PQFormat t2, PQFormat t3, PQFormat t4, PQFormat t5, PQFormat t6
  , PQFormat t7, PQFormat t8, PQFormat t9, PQFormat t10, PQFormat t11, PQFormat t12
  , PQFormat t13, PQFormat t14, PQFormat t15, PQFormat t16, PQFormat t17, PQFormat t18
  , PQFormat t19, PQFormat t20, PQFormat t21, PQFormat t22, PQFormat t23, PQFormat t24
  , PQFormat t25, PQFormat t26, PQFormat t27, PQFormat t28, PQFormat t29, PQFormat t30
  , PQFormat t31, PQFormat t32, PQFormat t33, PQFormat t34, PQFormat t35, PQFormat t36
  , PQFormat t37, PQFormat t38, PQFormat t39, PQFormat t40, PQFormat t41, PQFormat t42
  , PQFormat t43, PQFormat t44, PQFormat t45, PQFormat t46, PQFormat t47, PQFormat t48
  , PQFormat t49, PQFormat t50
  ) => PQFormat (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21,  t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50) where
    pqFormat = BS.concat [
        pqFormat @t1, pqFormat @t2, pqFormat @t3, pqFormat @t4
      , pqFormat @t5, pqFormat @t6, pqFormat @t7, pqFormat @t8
      , pqFormat @t9, pqFormat @t10, pqFormat @t11, pqFormat @t12
      , pqFormat @t13, pqFormat @t14, pqFormat @t15, pqFormat @t16
      , pqFormat @t17, pqFormat @t18, pqFormat @t19, pqFormat @t20
      , pqFormat @t21, pqFormat @t22, pqFormat @t23, pqFormat @t24
      , pqFormat @t25, pqFormat @t26, pqFormat @t27, pqFormat @t28
      , pqFormat @t29, pqFormat @t30, pqFormat @t31, pqFormat @t32
      , pqFormat @t33, pqFormat @t34, pqFormat @t35, pqFormat @t36
      , pqFormat @t37, pqFormat @t38, pqFormat @t39, pqFormat @t40
      , pqFormat @t41, pqFormat @t42, pqFormat @t43, pqFormat @t44
      , pqFormat @t45, pqFormat @t46, pqFormat @t47, pqFormat @t48
      , pqFormat @t49, pqFormat @t50
      ]
    pqVariables = 50
