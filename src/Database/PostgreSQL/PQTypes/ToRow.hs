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

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
        verify err =<< c_PQputf21 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 ->
        verify err =<< c_PQputf22 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 ->
        verify err =<< c_PQputf23 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
        verify err =<< c_PQputf24 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 ->
        verify err =<< c_PQputf25 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 ->
        verify err =<< c_PQputf26 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
        verify err =<< c_PQputf27 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 ->
        verify err =<< c_PQputf28 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 ->
        verify err =<< c_PQputf29 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
        verify err =<< c_PQputf30 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 ->
        verify err =<< c_PQputf31 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 ->
        verify err =<< c_PQputf32 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
        verify err =<< c_PQputf33 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 ->
        verify err =<< c_PQputf34 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34, ToSQL t35
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 -> toSQL t35 pa $ \p35 ->
        verify err =<< c_PQputf35 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34, ToSQL t35
  , ToSQL t36
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 -> toSQL t35 pa $ \p35 -> toSQL t36 pa $ \p36 ->
        verify err =<< c_PQputf36 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35,  t36) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34, ToSQL t35
  , ToSQL t36, ToSQL t37
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 -> toSQL t35 pa $ \p35 -> toSQL t36 pa $ \p36 ->
      toSQL t37 pa $ \p37 ->
        verify err =<< c_PQputf37 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35,  t36, t37) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34, ToSQL t35
  , ToSQL t36, ToSQL t37, ToSQL t38
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 -> toSQL t35 pa $ \p35 -> toSQL t36 pa $ \p36 ->
      toSQL t37 pa $ \p37 -> toSQL t38 pa $ \p38 ->
        verify err =<< c_PQputf38 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35,  t36, t37, t38) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34, ToSQL t35
  , ToSQL t36, ToSQL t37, ToSQL t38, ToSQL t39
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 -> toSQL t35 pa $ \p35 -> toSQL t36 pa $ \p36 ->
      toSQL t37 pa $ \p37 -> toSQL t38 pa $ \p38 -> toSQL t39 pa $ \p39 ->
        verify err =<< c_PQputf39 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35,  t36, t37, t38, t39) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34, ToSQL t35
  , ToSQL t36, ToSQL t37, ToSQL t38, ToSQL t39, ToSQL t40
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 -> toSQL t35 pa $ \p35 -> toSQL t36 pa $ \p36 ->
      toSQL t37 pa $ \p37 -> toSQL t38 pa $ \p38 -> toSQL t39 pa $ \p39 ->
      toSQL t40 pa $ \p40 ->
        verify err =<< c_PQputf40 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39 p40
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35,  t36, t37, t38, t39, t40) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34, ToSQL t35
  , ToSQL t36, ToSQL t37, ToSQL t38, ToSQL t39, ToSQL t40, ToSQL t41
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 -> toSQL t35 pa $ \p35 -> toSQL t36 pa $ \p36 ->
      toSQL t37 pa $ \p37 -> toSQL t38 pa $ \p38 -> toSQL t39 pa $ \p39 ->
      toSQL t40 pa $ \p40 -> toSQL t41 pa $ \p41 ->
        verify err =<< c_PQputf41 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39 p40 p41
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35,  t36, t37, t38, t39, t40, t41) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34, ToSQL t35
  , ToSQL t36, ToSQL t37, ToSQL t38, ToSQL t39, ToSQL t40, ToSQL t41, ToSQL t42
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 -> toSQL t35 pa $ \p35 -> toSQL t36 pa $ \p36 ->
      toSQL t37 pa $ \p37 -> toSQL t38 pa $ \p38 -> toSQL t39 pa $ \p39 ->
      toSQL t40 pa $ \p40 -> toSQL t41 pa $ \p41 -> toSQL t42 pa $ \p42 ->
        verify err =<< c_PQputf42 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39 p40 p41 p42
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35,  t36, t37, t38, t39, t40, t41, t42) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34, ToSQL t35
  , ToSQL t36, ToSQL t37, ToSQL t38, ToSQL t39, ToSQL t40, ToSQL t41, ToSQL t42
  , ToSQL t43
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 -> toSQL t35 pa $ \p35 -> toSQL t36 pa $ \p36 ->
      toSQL t37 pa $ \p37 -> toSQL t38 pa $ \p38 -> toSQL t39 pa $ \p39 ->
      toSQL t40 pa $ \p40 -> toSQL t41 pa $ \p41 -> toSQL t42 pa $ \p42 ->
      toSQL t43 pa $ \p43 ->
        verify err =<< c_PQputf43 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39 p40 p41 p42 p43
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35,  t36, t37, t38, t39, t40, t41, t42, t43) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34, ToSQL t35
  , ToSQL t36, ToSQL t37, ToSQL t38, ToSQL t39, ToSQL t40, ToSQL t41, ToSQL t42
  , ToSQL t43, ToSQL t44
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 -> toSQL t35 pa $ \p35 -> toSQL t36 pa $ \p36 ->
      toSQL t37 pa $ \p37 -> toSQL t38 pa $ \p38 -> toSQL t39 pa $ \p39 ->
      toSQL t40 pa $ \p40 -> toSQL t41 pa $ \p41 -> toSQL t42 pa $ \p42 ->
      toSQL t43 pa $ \p43 -> toSQL t44 pa $ \p44 ->
        verify err =<< c_PQputf44 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39 p40 p41 p42 p43 p44
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35,  t36, t37, t38, t39, t40, t41, t42, t43, t44) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34, ToSQL t35
  , ToSQL t36, ToSQL t37, ToSQL t38, ToSQL t39, ToSQL t40, ToSQL t41, ToSQL t42
  , ToSQL t43, ToSQL t44, ToSQL t45
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 -> toSQL t35 pa $ \p35 -> toSQL t36 pa $ \p36 ->
      toSQL t37 pa $ \p37 -> toSQL t38 pa $ \p38 -> toSQL t39 pa $ \p39 ->
      toSQL t40 pa $ \p40 -> toSQL t41 pa $ \p41 -> toSQL t42 pa $ \p42 ->
      toSQL t43 pa $ \p43 -> toSQL t44 pa $ \p44 -> toSQL t45 pa $ \p45 ->
        verify err =<< c_PQputf45 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39 p40 p41 p42 p43 p44 p45
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35,  t36, t37, t38, t39, t40, t41, t42, t43, t44, t45) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34, ToSQL t35
  , ToSQL t36, ToSQL t37, ToSQL t38, ToSQL t39, ToSQL t40, ToSQL t41, ToSQL t42
  , ToSQL t43, ToSQL t44, ToSQL t45, ToSQL t46
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 -> toSQL t35 pa $ \p35 -> toSQL t36 pa $ \p36 ->
      toSQL t37 pa $ \p37 -> toSQL t38 pa $ \p38 -> toSQL t39 pa $ \p39 ->
      toSQL t40 pa $ \p40 -> toSQL t41 pa $ \p41 -> toSQL t42 pa $ \p42 ->
      toSQL t43 pa $ \p43 -> toSQL t44 pa $ \p44 -> toSQL t45 pa $ \p45 ->
      toSQL t46 pa $ \p46 ->
        verify err =<< c_PQputf46 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39 p40 p41 p42 p43 p44 p45 p46
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35,  t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34, ToSQL t35
  , ToSQL t36, ToSQL t37, ToSQL t38, ToSQL t39, ToSQL t40, ToSQL t41, ToSQL t42
  , ToSQL t43, ToSQL t44, ToSQL t45, ToSQL t46, ToSQL t47
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 -> toSQL t35 pa $ \p35 -> toSQL t36 pa $ \p36 ->
      toSQL t37 pa $ \p37 -> toSQL t38 pa $ \p38 -> toSQL t39 pa $ \p39 ->
      toSQL t40 pa $ \p40 -> toSQL t41 pa $ \p41 -> toSQL t42 pa $ \p42 ->
      toSQL t43 pa $ \p43 -> toSQL t44 pa $ \p44 -> toSQL t45 pa $ \p45 ->
      toSQL t46 pa $ \p46 -> toSQL t47 pa $ \p47 ->
        verify err =<< c_PQputf47 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39 p40 p41 p42 p43 p44 p45 p46 p47
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35,  t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34, ToSQL t35
  , ToSQL t36, ToSQL t37, ToSQL t38, ToSQL t39, ToSQL t40, ToSQL t41, ToSQL t42
  , ToSQL t43, ToSQL t44, ToSQL t45, ToSQL t46, ToSQL t47, ToSQL t48
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 -> toSQL t35 pa $ \p35 -> toSQL t36 pa $ \p36 ->
      toSQL t37 pa $ \p37 -> toSQL t38 pa $ \p38 -> toSQL t39 pa $ \p39 ->
      toSQL t40 pa $ \p40 -> toSQL t41 pa $ \p41 -> toSQL t42 pa $ \p42 ->
      toSQL t43 pa $ \p43 -> toSQL t44 pa $ \p44 -> toSQL t45 pa $ \p45 ->
      toSQL t46 pa $ \p46 -> toSQL t47 pa $ \p47 -> toSQL t48 pa $ \p48 ->
        verify err =<< c_PQputf48 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39 p40 p41 p42 p43 p44 p45 p46 p47 p48
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35,  t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34, ToSQL t35
  , ToSQL t36, ToSQL t37, ToSQL t38, ToSQL t39, ToSQL t40, ToSQL t41, ToSQL t42
  , ToSQL t43, ToSQL t44, ToSQL t45, ToSQL t46, ToSQL t47, ToSQL t48, ToSQL t49
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 -> toSQL t35 pa $ \p35 -> toSQL t36 pa $ \p36 ->
      toSQL t37 pa $ \p37 -> toSQL t38 pa $ \p38 -> toSQL t39 pa $ \p39 ->
      toSQL t40 pa $ \p40 -> toSQL t41 pa $ \p41 -> toSQL t42 pa $ \p42 ->
      toSQL t43 pa $ \p43 -> toSQL t44 pa $ \p44 -> toSQL t45 pa $ \p45 ->
      toSQL t46 pa $ \p46 -> toSQL t47 pa $ \p47 -> toSQL t48 pa $ \p48 ->
      toSQL t49 pa $ \p49 ->
        verify err =<< c_PQputf49 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39 p40 p41 p42 p43 p44 p45 p46 p47 p48 p49
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35,  t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49) = row

instance (
    ToSQL t1, ToSQL t2, ToSQL t3, ToSQL t4, ToSQL t5, ToSQL t6, ToSQL t7
  , ToSQL t8, ToSQL t9, ToSQL t10, ToSQL t11, ToSQL t12, ToSQL t13, ToSQL t14
  , ToSQL t15, ToSQL t16, ToSQL t17, ToSQL t18, ToSQL t19, ToSQL t20, ToSQL t21
  , ToSQL t22, ToSQL t23, ToSQL t24, ToSQL t25, ToSQL t26, ToSQL t27, ToSQL t28
  , ToSQL t29, ToSQL t30, ToSQL t31, ToSQL t32, ToSQL t33, ToSQL t34, ToSQL t35
  , ToSQL t36, ToSQL t37, ToSQL t38, ToSQL t39, ToSQL t40, ToSQL t41, ToSQL t42
  , ToSQL t43, ToSQL t44, ToSQL t45, ToSQL t46, ToSQL t47, ToSQL t48, ToSQL t49
  , ToSQL t50
  ) => ToRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50) where
    toRow row pa param err fmt =
      toSQL t1 pa $ \p1 -> toSQL t2 pa $ \p2 -> toSQL t3 pa $ \p3 ->
      toSQL t4 pa $ \p4 -> toSQL t5 pa $ \p5 -> toSQL t6 pa $ \p6 ->
      toSQL t7 pa $ \p7 -> toSQL t8 pa $ \p8 -> toSQL t9 pa $ \p9 ->
      toSQL t10 pa $ \p10 -> toSQL t11 pa $ \p11 -> toSQL t12 pa $ \p12 ->
      toSQL t13 pa $ \p13 -> toSQL t14 pa $ \p14 -> toSQL t15 pa $ \p15 ->
      toSQL t16 pa $ \p16 -> toSQL t17 pa $ \p17 -> toSQL t18 pa $ \p18 ->
      toSQL t19 pa $ \p19 -> toSQL t20 pa $ \p20 -> toSQL t21 pa $ \p21 ->
      toSQL t22 pa $ \p22 -> toSQL t23 pa $ \p23 -> toSQL t24 pa $ \p24 ->
      toSQL t25 pa $ \p25 -> toSQL t26 pa $ \p26 -> toSQL t27 pa $ \p27 ->
      toSQL t28 pa $ \p28 -> toSQL t29 pa $ \p29 -> toSQL t30 pa $ \p30 ->
      toSQL t31 pa $ \p31 -> toSQL t32 pa $ \p32 -> toSQL t33 pa $ \p33 ->
      toSQL t34 pa $ \p34 -> toSQL t35 pa $ \p35 -> toSQL t36 pa $ \p36 ->
      toSQL t37 pa $ \p37 -> toSQL t38 pa $ \p38 -> toSQL t39 pa $ \p39 ->
      toSQL t40 pa $ \p40 -> toSQL t41 pa $ \p41 -> toSQL t42 pa $ \p42 ->
      toSQL t43 pa $ \p43 -> toSQL t44 pa $ \p44 -> toSQL t45 pa $ \p45 ->
      toSQL t46 pa $ \p46 -> toSQL t47 pa $ \p47 -> toSQL t48 pa $ \p48 ->
      toSQL t49 pa $ \p49 -> toSQL t50 pa $ \p50 ->
        verify err =<< c_PQputf50 param err fmt p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39 p40 p41 p42 p43 p44 p45 p46 p47 p48 p49 p50
      where
        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35,  t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50) = row
