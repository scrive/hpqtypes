{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, Rank2Types, ScopedTypeVariables #-}
module Database.PostgreSQL.PQTypes.ToRow (
    ToRow(..)
  , toRow'
  ) where

import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import qualified Data.ByteString as BS

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.Internal.C.Put
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.Single
import Database.PostgreSQL.PQTypes.ToSQL

-- | 'verifyPQTRes' specialized for usage in 'toRow'.
verify :: Ptr PGerror -> CInt -> IO ()
verify err = verifyPQTRes err "toRow"

----------------------------------------

-- | More convenient version of 'toRow' that
-- allocates 'PGerror' and format string by itself.
toRow' :: forall row. ToRow row => row -> ParamAllocator -> Ptr PGparam -> IO ()
toRow' row pa param = alloca $ \err ->
  BS.useAsCString (pqFormat row) (toRow row pa param err)

-- | Class which represents \"from Haskell tuple to SQL row\" transformation.
class PQFormat row => ToRow row where
  -- | Put supplied tuple into 'PGparam' using given format string.
  toRow :: row            -- ^ Tuple to be put into 'PGparam'.
        -> ParamAllocator -- ^ 'PGparam' allocator for 'toSQL'.
        -> Ptr PGparam    -- ^ 'PGparam' to put tuple into.
        -> Ptr PGerror    -- ^ Local error info.
        -> CString        -- ^ Format of a tuple to be put.
        -> IO ()

instance ToRow () where
  toRow _ _ _ _ _ = return ()

instance ToSQL t => ToRow (Single t) where
  toRow (Single t) pa param err fmt = toSQL t pa $ \base ->
    verify err =<< c_PQputf1 param err fmt base

instance (
    ToSQL t1, ToSQL t2
  ) => ToRow (t1, t2) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 ->
        verify err =<< c_PQputf2 param err fmt p1 p2
      where
        (t1, t2) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3
  ) => ToRow (t1, t2, t3) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
        verify err =<< c_PQputf3 param err fmt p1 p2 p3
      where
        (t1, t2, t3) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4
  ) => ToRow (t1, t2, t3, t4) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 ->
        verify err =<< c_PQputf4 param err fmt p1 p2 p3 p4
      where
        (t1, t2, t3, t4) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5
  ) => ToRow (t1, t2, t3, t4, t5) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 ->
        verify err =<< c_PQputf5 param err fmt p1 p2 p3 p4 p5
      where
        (t1, t2, t3, t4, t5) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6
  ) => ToRow (t1, t2, t3, t4, t5, t6) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
        verify err =<< c_PQputf6 param err fmt p1 p2 p3 p4 p5 p6
      where
        (t1, t2, t3, t4, t5, t6) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 ->
        verify err =<< c_PQputf7 param err fmt p1 p2 p3 p4 p5 p6 p7
      where
        (t1, t2, t3, t4, t5, t6, t7) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 ->
        verify err =<< c_PQputf8 param err fmt p1 p2 p3 p4 p5 p6 p7 p8
      where
        (t1, t2, t3, t4, t5, t6, t7, t8) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
        verify err =<< c_PQputf9 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 ->
        verify err =<< c_PQputf10 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 ->
        verify err =<< c_PQputf11 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
        verify err =<< c_PQputf12 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 ->
        verify err =<< c_PQputf13 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 ->
        verify err =<< c_PQputf14 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
        verify err =<< c_PQputf15 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 ->
        verify err =<< c_PQputf16 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 ->
        verify err =<< c_PQputf17 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
        verify err =<< c_PQputf18 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 ->
        verify err =<< c_PQputf19 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 ->
        verify err =<< c_PQputf20 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) = row
