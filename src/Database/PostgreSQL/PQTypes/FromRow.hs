{-# LANGUAGE TypeApplications #-}
module Database.PostgreSQL.PQTypes.FromRow (
    FromRow(..)
  , fromRow'
  ) where

import Data.Functor.Identity
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Control.Exception as E
import qualified Data.ByteString.Unsafe as BS

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.C.Get
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Utils

-- | Convert base (libpqtypes) type to destination type.
convert :: FromSQL t => Ptr PGresult -> CInt -> CInt -> PQBase t -> IO t
convert res tuple column base = do
  isNull <- c_PQgetisnull res tuple column
  fromSQL (if isNull == 1 then Nothing else Just base)
    `E.catch` rethrowWithConvError
  where
    rethrowWithConvError :: E.SomeException -> IO a
    rethrowWithConvError (E.SomeException e) = do
      colname <- safePeekCString' =<< c_PQfname res column
      E.throwIO ConversionError {
        convColumn = fromIntegral column + 1
      , convColumnName = colname
      , convRow = fromIntegral tuple + 1
      , convError = e
      }

-- | 'verifyPQTRes' specialized for usage in 'fromRow'.
verify :: Ptr PGerror -> CInt -> IO ()
verify err = verifyPQTRes err "fromRow"

withFormat :: forall row. FromRow row => (CString -> IO row) -> IO row
withFormat = BS.unsafeUseAsCString $ pqFormat0 @row

----------------------------------------

-- | More convenient version of 'fromRow' that allocates 'PGerror' by itself.
fromRow' :: forall row. FromRow row => Ptr PGresult -> CInt -> CInt -> IO row
fromRow' res b i = alloca $ \err -> fromRow res err b i

-- | Class which represents \"from SQL row to Haskell tuple\" transformation.
class PQFormat row => FromRow row where
  -- | Extract SQL row from 'PGresult' and convert it into a tuple.
  fromRow  :: Ptr PGresult -- ^ Source result.
           -> Ptr PGerror  -- ^ Local error info.
           -> CInt         -- ^ Base position for c_PQgetf.
           -> CInt         -- ^ Index of row to be extracted.
           -> IO row

instance (
    FromRow row1, FromRow row2
  ) => FromRow (row1 :*: row2) where
    fromRow res err b i = (:*:)
      <$> fromRow res err b  i
      <*> fromRow res err b' i
      where
        b' = b + fromIntegral (pqVariables @row1)

instance FromRow () where
  fromRow _ _ _ _ = return ()

instance FromSQL t => FromRow (Identity t) where
  fromRow res err b i = withFormat $ \fmt -> alloca $ \p1 -> do
    verify err =<< c_PQgetf1 res err i fmt b p1
    t <- peek p1 >>= convert res i b
    return (Identity t)

instance (
    FromSQL t1, FromSQL t2
  ) => FromRow (t1, t2) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> do
        verify err =<< c_PQgetf2 res err i fmt b p0 (b+1) p1
        (,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3
  ) => FromRow (t1, t2, t3) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> do
        verify err =<< c_PQgetf3 res err i fmt b p0 (b+1) p1 (b+2) p2
        (,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4
  ) => FromRow (t1, t2, t3, t4) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> do
        verify err =<< c_PQgetf4 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3
        (,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5
  ) => FromRow (t1, t2, t3, t4, t5) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 -> do
        verify err =<< c_PQgetf5 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4
        (,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  ) => FromRow (t1, t2, t3, t4, t5, t6) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> do
        verify err =<< c_PQgetf6 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5
        (,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> do
        verify err =<< c_PQgetf7 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6
        (,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> do
        verify err =<< c_PQgetf8 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7
        (,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> do
        verify err =<< c_PQgetf9 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8
        (,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 -> do
        verify err =<< c_PQgetf10 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9
        (,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> do
        verify err =<< c_PQgetf11 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10
        (,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> do
        verify err =<< c_PQgetf12 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11
        (,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> do
        verify err =<< c_PQgetf13 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12
        (,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> do
        verify err =<< c_PQgetf14 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13
        (,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 -> do
        verify err =<< c_PQgetf15 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14
        (,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> do
        verify err =<< c_PQgetf16 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15
        (,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> do
        verify err =<< c_PQgetf17 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16
        (,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> do
        verify err =<< c_PQgetf18 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17
        (,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> do
        verify err =<< c_PQgetf19 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18
        (,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 -> do
        verify err =<< c_PQgetf20 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19
        (,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> do
        verify err =<< c_PQgetf21 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20
        (,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> do
        verify err =<< c_PQgetf22 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21
        (,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> do
        verify err =<< c_PQgetf23 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22
        (,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> do
        verify err =<< c_PQgetf24 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23
        (,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 -> do
        verify err =<< c_PQgetf25 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24
        (,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> do
        verify err =<< c_PQgetf26 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25
        (,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> do
        verify err =<< c_PQgetf27 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26
        (,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> do
        verify err =<< c_PQgetf28 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> do
        verify err =<< c_PQgetf29 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 -> do
        verify err =<< c_PQgetf30 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> do
        verify err =<< c_PQgetf31 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> do
        verify err =<< c_PQgetf32 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> do
        verify err =<< c_PQgetf33 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> do
        verify err =<< c_PQgetf34 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34, FromSQL t35
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> alloca $ \p34 -> do
        verify err =<< c_PQgetf35 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33 (b+34) p34
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))
          <*> (peek p34 >>= convert res i (b+34))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34, FromSQL t35, FromSQL t36
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> alloca $ \p34 ->
      alloca $ \p35 -> do
        verify err =<< c_PQgetf36 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33 (b+34) p34 (b+35) p35
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))
          <*> (peek p34 >>= convert res i (b+34)) <*> (peek p35 >>= convert res i (b+35))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34, FromSQL t35, FromSQL t36
  , FromSQL t37
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> alloca $ \p34 ->
      alloca $ \p35 -> alloca $ \p36 -> do
        verify err =<< c_PQgetf37 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33 (b+34) p34 (b+35) p35 (b+36) p36
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))
          <*> (peek p34 >>= convert res i (b+34)) <*> (peek p35 >>= convert res i (b+35))
          <*> (peek p36 >>= convert res i (b+36))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34, FromSQL t35, FromSQL t36
  , FromSQL t37, FromSQL t38
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> alloca $ \p34 ->
      alloca $ \p35 -> alloca $ \p36 -> alloca $ \p37 -> do
        verify err =<< c_PQgetf38 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33 (b+34) p34 (b+35) p35 (b+36) p36 (b+37) p37
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))
          <*> (peek p34 >>= convert res i (b+34)) <*> (peek p35 >>= convert res i (b+35))
          <*> (peek p36 >>= convert res i (b+36)) <*> (peek p37 >>= convert res i (b+37))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34, FromSQL t35, FromSQL t36
  , FromSQL t37, FromSQL t38, FromSQL t39
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> alloca $ \p34 ->
      alloca $ \p35 -> alloca $ \p36 -> alloca $ \p37 -> alloca $ \p38 -> do
        verify err =<< c_PQgetf39 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33 (b+34) p34 (b+35) p35 (b+36) p36 (b+37) p37 (b+38) p38
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))
          <*> (peek p34 >>= convert res i (b+34)) <*> (peek p35 >>= convert res i (b+35))
          <*> (peek p36 >>= convert res i (b+36)) <*> (peek p37 >>= convert res i (b+37))
          <*> (peek p38 >>= convert res i (b+38))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34, FromSQL t35, FromSQL t36
  , FromSQL t37, FromSQL t38, FromSQL t39, FromSQL t40
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> alloca $ \p34 ->
      alloca $ \p35 -> alloca $ \p36 -> alloca $ \p37 -> alloca $ \p38 -> alloca $ \p39 -> do
        verify err =<< c_PQgetf40 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33 (b+34) p34 (b+35) p35 (b+36) p36 (b+37) p37 (b+38) p38 (b+39) p39
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))
          <*> (peek p34 >>= convert res i (b+34)) <*> (peek p35 >>= convert res i (b+35))
          <*> (peek p36 >>= convert res i (b+36)) <*> (peek p37 >>= convert res i (b+37))
          <*> (peek p38 >>= convert res i (b+38)) <*> (peek p39 >>= convert res i (b+39))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34, FromSQL t35, FromSQL t36
  , FromSQL t37, FromSQL t38, FromSQL t39, FromSQL t40, FromSQL t41
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> alloca $ \p34 ->
      alloca $ \p35 -> alloca $ \p36 -> alloca $ \p37 -> alloca $ \p38 -> alloca $ \p39 ->
      alloca $ \p40 -> do
        verify err =<< c_PQgetf41 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33 (b+34) p34 (b+35) p35 (b+36) p36 (b+37) p37 (b+38) p38 (b+39) p39 (b+40) p40
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))
          <*> (peek p34 >>= convert res i (b+34)) <*> (peek p35 >>= convert res i (b+35))
          <*> (peek p36 >>= convert res i (b+36)) <*> (peek p37 >>= convert res i (b+37))
          <*> (peek p38 >>= convert res i (b+38)) <*> (peek p39 >>= convert res i (b+39))
          <*> (peek p40 >>= convert res i (b+40))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34, FromSQL t35, FromSQL t36
  , FromSQL t37, FromSQL t38, FromSQL t39, FromSQL t40, FromSQL t41, FromSQL t42
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> alloca $ \p34 ->
      alloca $ \p35 -> alloca $ \p36 -> alloca $ \p37 -> alloca $ \p38 -> alloca $ \p39 ->
      alloca $ \p40 -> alloca $ \p41 -> do
        verify err =<< c_PQgetf42 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33 (b+34) p34 (b+35) p35 (b+36) p36 (b+37) p37 (b+38) p38 (b+39) p39 (b+40) p40 (b+41) p41
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))
          <*> (peek p34 >>= convert res i (b+34)) <*> (peek p35 >>= convert res i (b+35))
          <*> (peek p36 >>= convert res i (b+36)) <*> (peek p37 >>= convert res i (b+37))
          <*> (peek p38 >>= convert res i (b+38)) <*> (peek p39 >>= convert res i (b+39))
          <*> (peek p40 >>= convert res i (b+40)) <*> (peek p41 >>= convert res i (b+41))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34, FromSQL t35, FromSQL t36
  , FromSQL t37, FromSQL t38, FromSQL t39, FromSQL t40, FromSQL t41, FromSQL t42
  , FromSQL t43
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> alloca $ \p34 ->
      alloca $ \p35 -> alloca $ \p36 -> alloca $ \p37 -> alloca $ \p38 -> alloca $ \p39 ->
      alloca $ \p40 -> alloca $ \p41 -> alloca $ \p42 -> do
        verify err =<< c_PQgetf43 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33 (b+34) p34 (b+35) p35 (b+36) p36 (b+37) p37 (b+38) p38 (b+39) p39 (b+40) p40 (b+41) p41 (b+42) p42
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))
          <*> (peek p34 >>= convert res i (b+34)) <*> (peek p35 >>= convert res i (b+35))
          <*> (peek p36 >>= convert res i (b+36)) <*> (peek p37 >>= convert res i (b+37))
          <*> (peek p38 >>= convert res i (b+38)) <*> (peek p39 >>= convert res i (b+39))
          <*> (peek p40 >>= convert res i (b+40)) <*> (peek p41 >>= convert res i (b+41))
          <*> (peek p42 >>= convert res i (b+42))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34, FromSQL t35, FromSQL t36
  , FromSQL t37, FromSQL t38, FromSQL t39, FromSQL t40, FromSQL t41, FromSQL t42
  , FromSQL t43, FromSQL t44
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> alloca $ \p34 ->
      alloca $ \p35 -> alloca $ \p36 -> alloca $ \p37 -> alloca $ \p38 -> alloca $ \p39 ->
      alloca $ \p40 -> alloca $ \p41 -> alloca $ \p42 -> alloca $ \p43 -> do
        verify err =<< c_PQgetf44 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33 (b+34) p34 (b+35) p35 (b+36) p36 (b+37) p37 (b+38) p38 (b+39) p39 (b+40) p40 (b+41) p41 (b+42) p42 (b+43) p43
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))
          <*> (peek p34 >>= convert res i (b+34)) <*> (peek p35 >>= convert res i (b+35))
          <*> (peek p36 >>= convert res i (b+36)) <*> (peek p37 >>= convert res i (b+37))
          <*> (peek p38 >>= convert res i (b+38)) <*> (peek p39 >>= convert res i (b+39))
          <*> (peek p40 >>= convert res i (b+40)) <*> (peek p41 >>= convert res i (b+41))
          <*> (peek p42 >>= convert res i (b+42)) <*> (peek p43 >>= convert res i (b+43))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34, FromSQL t35, FromSQL t36
  , FromSQL t37, FromSQL t38, FromSQL t39, FromSQL t40, FromSQL t41, FromSQL t42
  , FromSQL t43, FromSQL t44, FromSQL t45
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> alloca $ \p34 ->
      alloca $ \p35 -> alloca $ \p36 -> alloca $ \p37 -> alloca $ \p38 -> alloca $ \p39 ->
      alloca $ \p40 -> alloca $ \p41 -> alloca $ \p42 -> alloca $ \p43 -> alloca $ \p44 -> do
        verify err =<< c_PQgetf45 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33 (b+34) p34 (b+35) p35 (b+36) p36 (b+37) p37 (b+38) p38 (b+39) p39 (b+40) p40 (b+41) p41 (b+42) p42 (b+43) p43 (b+44) p44
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))
          <*> (peek p34 >>= convert res i (b+34)) <*> (peek p35 >>= convert res i (b+35))
          <*> (peek p36 >>= convert res i (b+36)) <*> (peek p37 >>= convert res i (b+37))
          <*> (peek p38 >>= convert res i (b+38)) <*> (peek p39 >>= convert res i (b+39))
          <*> (peek p40 >>= convert res i (b+40)) <*> (peek p41 >>= convert res i (b+41))
          <*> (peek p42 >>= convert res i (b+42)) <*> (peek p43 >>= convert res i (b+43))
          <*> (peek p44 >>= convert res i (b+44))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34, FromSQL t35, FromSQL t36
  , FromSQL t37, FromSQL t38, FromSQL t39, FromSQL t40, FromSQL t41, FromSQL t42
  , FromSQL t43, FromSQL t44, FromSQL t45, FromSQL t46
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> alloca $ \p34 ->
      alloca $ \p35 -> alloca $ \p36 -> alloca $ \p37 -> alloca $ \p38 -> alloca $ \p39 ->
      alloca $ \p40 -> alloca $ \p41 -> alloca $ \p42 -> alloca $ \p43 -> alloca $ \p44 ->
      alloca $ \p45 -> do
        verify err =<< c_PQgetf46 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33 (b+34) p34 (b+35) p35 (b+36) p36 (b+37) p37 (b+38) p38 (b+39) p39 (b+40) p40 (b+41) p41 (b+42) p42 (b+43) p43 (b+44) p44 (b+45) p45
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))
          <*> (peek p34 >>= convert res i (b+34)) <*> (peek p35 >>= convert res i (b+35))
          <*> (peek p36 >>= convert res i (b+36)) <*> (peek p37 >>= convert res i (b+37))
          <*> (peek p38 >>= convert res i (b+38)) <*> (peek p39 >>= convert res i (b+39))
          <*> (peek p40 >>= convert res i (b+40)) <*> (peek p41 >>= convert res i (b+41))
          <*> (peek p42 >>= convert res i (b+42)) <*> (peek p43 >>= convert res i (b+43))
          <*> (peek p44 >>= convert res i (b+44)) <*> (peek p45 >>= convert res i (b+45))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34, FromSQL t35, FromSQL t36
  , FromSQL t37, FromSQL t38, FromSQL t39, FromSQL t40, FromSQL t41, FromSQL t42
  , FromSQL t43, FromSQL t44, FromSQL t45, FromSQL t46, FromSQL t47
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> alloca $ \p34 ->
      alloca $ \p35 -> alloca $ \p36 -> alloca $ \p37 -> alloca $ \p38 -> alloca $ \p39 ->
      alloca $ \p40 -> alloca $ \p41 -> alloca $ \p42 -> alloca $ \p43 -> alloca $ \p44 ->
      alloca $ \p45 -> alloca $ \p46 -> do
        verify err =<< c_PQgetf47 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33 (b+34) p34 (b+35) p35 (b+36) p36 (b+37) p37 (b+38) p38 (b+39) p39 (b+40) p40 (b+41) p41 (b+42) p42 (b+43) p43 (b+44) p44 (b+45) p45 (b+46) p46
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))
          <*> (peek p34 >>= convert res i (b+34)) <*> (peek p35 >>= convert res i (b+35))
          <*> (peek p36 >>= convert res i (b+36)) <*> (peek p37 >>= convert res i (b+37))
          <*> (peek p38 >>= convert res i (b+38)) <*> (peek p39 >>= convert res i (b+39))
          <*> (peek p40 >>= convert res i (b+40)) <*> (peek p41 >>= convert res i (b+41))
          <*> (peek p42 >>= convert res i (b+42)) <*> (peek p43 >>= convert res i (b+43))
          <*> (peek p44 >>= convert res i (b+44)) <*> (peek p45 >>= convert res i (b+45))
          <*> (peek p46 >>= convert res i (b+46))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34, FromSQL t35, FromSQL t36
  , FromSQL t37, FromSQL t38, FromSQL t39, FromSQL t40, FromSQL t41, FromSQL t42
  , FromSQL t43, FromSQL t44, FromSQL t45, FromSQL t46, FromSQL t47, FromSQL t48
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> alloca $ \p34 ->
      alloca $ \p35 -> alloca $ \p36 -> alloca $ \p37 -> alloca $ \p38 -> alloca $ \p39 ->
      alloca $ \p40 -> alloca $ \p41 -> alloca $ \p42 -> alloca $ \p43 -> alloca $ \p44 ->
      alloca $ \p45 -> alloca $ \p46 -> alloca $ \p47 -> do
        verify err =<< c_PQgetf48 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33 (b+34) p34 (b+35) p35 (b+36) p36 (b+37) p37 (b+38) p38 (b+39) p39 (b+40) p40 (b+41) p41 (b+42) p42 (b+43) p43 (b+44) p44 (b+45) p45 (b+46) p46 (b+47) p47
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))
          <*> (peek p34 >>= convert res i (b+34)) <*> (peek p35 >>= convert res i (b+35))
          <*> (peek p36 >>= convert res i (b+36)) <*> (peek p37 >>= convert res i (b+37))
          <*> (peek p38 >>= convert res i (b+38)) <*> (peek p39 >>= convert res i (b+39))
          <*> (peek p40 >>= convert res i (b+40)) <*> (peek p41 >>= convert res i (b+41))
          <*> (peek p42 >>= convert res i (b+42)) <*> (peek p43 >>= convert res i (b+43))
          <*> (peek p44 >>= convert res i (b+44)) <*> (peek p45 >>= convert res i (b+45))
          <*> (peek p46 >>= convert res i (b+46)) <*> (peek p47 >>= convert res i (b+47))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34, FromSQL t35, FromSQL t36
  , FromSQL t37, FromSQL t38, FromSQL t39, FromSQL t40, FromSQL t41, FromSQL t42
  , FromSQL t43, FromSQL t44, FromSQL t45, FromSQL t46, FromSQL t47, FromSQL t48
  , FromSQL t49
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> alloca $ \p34 ->
      alloca $ \p35 -> alloca $ \p36 -> alloca $ \p37 -> alloca $ \p38 -> alloca $ \p39 ->
      alloca $ \p40 -> alloca $ \p41 -> alloca $ \p42 -> alloca $ \p43 -> alloca $ \p44 ->
      alloca $ \p45 -> alloca $ \p46 -> alloca $ \p47 -> alloca $ \p48 -> do
        verify err =<< c_PQgetf49 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33 (b+34) p34 (b+35) p35 (b+36) p36 (b+37) p37 (b+38) p38 (b+39) p39 (b+40) p40 (b+41) p41 (b+42) p42 (b+43) p43 (b+44) p44 (b+45) p45 (b+46) p46 (b+47) p47 (b+48) p48
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))
          <*> (peek p34 >>= convert res i (b+34)) <*> (peek p35 >>= convert res i (b+35))
          <*> (peek p36 >>= convert res i (b+36)) <*> (peek p37 >>= convert res i (b+37))
          <*> (peek p38 >>= convert res i (b+38)) <*> (peek p39 >>= convert res i (b+39))
          <*> (peek p40 >>= convert res i (b+40)) <*> (peek p41 >>= convert res i (b+41))
          <*> (peek p42 >>= convert res i (b+42)) <*> (peek p43 >>= convert res i (b+43))
          <*> (peek p44 >>= convert res i (b+44)) <*> (peek p45 >>= convert res i (b+45))
          <*> (peek p46 >>= convert res i (b+46)) <*> (peek p47 >>= convert res i (b+47))
          <*> (peek p48 >>= convert res i (b+48))

instance (
    FromSQL t1, FromSQL t2, FromSQL t3, FromSQL t4, FromSQL t5, FromSQL t6
  , FromSQL t7, FromSQL t8, FromSQL t9, FromSQL t10, FromSQL t11, FromSQL t12
  , FromSQL t13, FromSQL t14, FromSQL t15, FromSQL t16, FromSQL t17, FromSQL t18
  , FromSQL t19, FromSQL t20, FromSQL t21, FromSQL t22, FromSQL t23, FromSQL t24
  , FromSQL t25, FromSQL t26, FromSQL t27, FromSQL t28, FromSQL t29, FromSQL t30
  , FromSQL t31, FromSQL t32, FromSQL t33, FromSQL t34, FromSQL t35, FromSQL t36
  , FromSQL t37, FromSQL t38, FromSQL t39, FromSQL t40, FromSQL t41, FromSQL t42
  , FromSQL t43, FromSQL t44, FromSQL t45, FromSQL t46, FromSQL t47, FromSQL t48
  , FromSQL t49, FromSQL t50
  ) => FromRow (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50) where
    fromRow res err b i = withFormat $ \fmt ->
      alloca $ \p0 -> alloca $ \p1 -> alloca $ \p2 -> alloca $ \p3 -> alloca $ \p4 ->
      alloca $ \p5 -> alloca $ \p6 -> alloca $ \p7 -> alloca $ \p8 -> alloca $ \p9 ->
      alloca $ \p10 -> alloca $ \p11 -> alloca $ \p12 -> alloca $ \p13 -> alloca $ \p14 ->
      alloca $ \p15 -> alloca $ \p16 -> alloca $ \p17 -> alloca $ \p18 -> alloca $ \p19 ->
      alloca $ \p20 -> alloca $ \p21 -> alloca $ \p22 -> alloca $ \p23 -> alloca $ \p24 ->
      alloca $ \p25 -> alloca $ \p26 -> alloca $ \p27 -> alloca $ \p28 -> alloca $ \p29 ->
      alloca $ \p30 -> alloca $ \p31 -> alloca $ \p32 -> alloca $ \p33 -> alloca $ \p34 ->
      alloca $ \p35 -> alloca $ \p36 -> alloca $ \p37 -> alloca $ \p38 -> alloca $ \p39 ->
      alloca $ \p40 -> alloca $ \p41 -> alloca $ \p42 -> alloca $ \p43 -> alloca $ \p44 ->
      alloca $ \p45 -> alloca $ \p46 -> alloca $ \p47 -> alloca $ \p48 -> alloca $ \p49 -> do
        verify err =<< c_PQgetf50 res err i fmt b p0 (b+1) p1 (b+2) p2 (b+3) p3 (b+4) p4 (b+5) p5 (b+6) p6 (b+7) p7 (b+8) p8 (b+9) p9 (b+10) p10 (b+11) p11 (b+12) p12 (b+13) p13 (b+14) p14 (b+15) p15 (b+16) p16 (b+17) p17 (b+18) p18 (b+19) p19 (b+20) p20 (b+21) p21 (b+22) p22 (b+23) p23 (b+24) p24 (b+25) p25 (b+26) p26 (b+27) p27 (b+28) p28 (b+29) p29 (b+30) p30 (b+31) p31 (b+32) p32 (b+33) p33 (b+34) p34 (b+35) p35 (b+36) p36 (b+37) p37 (b+38) p38 (b+39) p39 (b+40) p40 (b+41) p41 (b+42) p42 (b+43) p43 (b+44) p44 (b+45) p45 (b+46) p46 (b+47) p47 (b+48) p48 (b+49) p49
        (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
          <$> (peek p0 >>= convert res i b) <*> (peek p1 >>= convert res i (b+1))
          <*> (peek p2 >>= convert res i (b+2)) <*> (peek p3 >>= convert res i (b+3))
          <*> (peek p4 >>= convert res i (b+4)) <*> (peek p5 >>= convert res i (b+5))
          <*> (peek p6 >>= convert res i (b+6)) <*> (peek p7 >>= convert res i (b+7))
          <*> (peek p8 >>= convert res i (b+8)) <*> (peek p9 >>= convert res i (b+9))
          <*> (peek p10 >>= convert res i (b+10)) <*> (peek p11 >>= convert res i (b+11))
          <*> (peek p12 >>= convert res i (b+12)) <*> (peek p13 >>= convert res i (b+13))
          <*> (peek p14 >>= convert res i (b+14)) <*> (peek p15 >>= convert res i (b+15))
          <*> (peek p16 >>= convert res i (b+16)) <*> (peek p17 >>= convert res i (b+17))
          <*> (peek p18 >>= convert res i (b+18)) <*> (peek p19 >>= convert res i (b+19))
          <*> (peek p20 >>= convert res i (b+20)) <*> (peek p21 >>= convert res i (b+21))
          <*> (peek p22 >>= convert res i (b+22)) <*> (peek p23 >>= convert res i (b+23))
          <*> (peek p24 >>= convert res i (b+24)) <*> (peek p25 >>= convert res i (b+25))
          <*> (peek p26 >>= convert res i (b+26)) <*> (peek p27 >>= convert res i (b+27))
          <*> (peek p28 >>= convert res i (b+28)) <*> (peek p29 >>= convert res i (b+29))
          <*> (peek p30 >>= convert res i (b+30)) <*> (peek p31 >>= convert res i (b+31))
          <*> (peek p32 >>= convert res i (b+32)) <*> (peek p33 >>= convert res i (b+33))
          <*> (peek p34 >>= convert res i (b+34)) <*> (peek p35 >>= convert res i (b+35))
          <*> (peek p36 >>= convert res i (b+36)) <*> (peek p37 >>= convert res i (b+37))
          <*> (peek p38 >>= convert res i (b+38)) <*> (peek p39 >>= convert res i (b+39))
          <*> (peek p40 >>= convert res i (b+40)) <*> (peek p41 >>= convert res i (b+41))
          <*> (peek p42 >>= convert res i (b+42)) <*> (peek p43 >>= convert res i (b+43))
          <*> (peek p44 >>= convert res i (b+44)) <*> (peek p45 >>= convert res i (b+45))
          <*> (peek p46 >>= convert res i (b+46)) <*> (peek p47 >>= convert res i (b+47))
          <*> (peek p48 >>= convert res i (b+48)) <*> (peek p49 >>= convert res i (b+49))
