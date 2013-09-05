{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses
  , OverloadedStrings, ScopedTypeVariables, UndecidableInstances #-}
module Database.PostgreSQL.ToSQL (ToSQL(..)) where

import Control.Applicative
import Data.Int
import Data.Text (Text)
import Data.Text.Encoding
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as V

import Database.PostgreSQL.Internal.C.Interface
import Database.PostgreSQL.Internal.C.Put
import Database.PostgreSQL.Internal.C.Types
--import Database.PostgreSQL.Internal.Exception
import Database.PostgreSQL.Types

class Storable base => ToSQL dest base | dest -> base where
  pqFormatPut :: dest -> BS.ByteString
  toSQL :: Ptr PGconn -> dest -> (Either String (Maybe base) -> IO r) -> IO r

-- NULLables

instance ToSQL dest base => ToSQL (Maybe dest) base where
  pqFormatPut _ = pqFormatPut (undefined::dest)
  toSQL conn mdest conv = case mdest of
    Nothing   -> conv (Right Nothing)
    Just dest -> toSQL conn dest conv

-- NUMERICS

instance ToSQL Int16 CShort where
  pqFormatPut _ = "%int2"
  toSQL _ n conv = conv . Right . Just . fromIntegral $ n

instance ToSQL Int32 CInt where
  pqFormatPut _ = "%int4"
  toSQL _ n conv = conv . Right . Just . fromIntegral $ n

instance ToSQL Int64 CLLong where
  pqFormatPut _ = "%int8"
  toSQL _ n conv = conv . Right . Just . fromIntegral $ n

instance ToSQL Float CFloat where
  pqFormatPut _ = "%float4"
  toSQL _ n conv = conv . Right . Just . realToFrac $ n

instance ToSQL Double CDouble where
  pqFormatPut _ = "%float8"
  toSQL _ n conv = conv . Right . Just . realToFrac $ n

-- ARRAYS

instance (ToSQL dest base, PQPut base) => ToSQL (Array dest) (Ptr PGarray) where
  pqFormatPut _ = pqFormatPut (undefined::dest) `BS.append` "[]"
  toSQL conn (Array arr) conv = alloca $ \ptr -> do
    fparam <- c_safePQparamCreate conn
    withForeignPtr fparam $ \param -> do
      eok <- BS.useAsCString (pqFormatPut (undefined::dest)) $ putValues arr param
      case eok of
        Left msg -> conv $ Left msg
        Right () -> do
          poke ptr PGarray {
            pgArrayNDims = 1
          , pgArrayLBound = V.empty
          , pgArrayDims = V.empty
          , pgArrayParam = param
          , pgArrayRes = nullPtr
          }
          conv . Right . Just $ ptr
    where
      putValues [] _ _ = return . Right $ ()
      putValues (item : rest) param fmt = do
        esuccess <- toSQL conn item $ \embase -> case embase of
          Left msg          -> return . Left $ msg
          Right Nothing     -> Right <$> c_PQPutfNULL param
          Right (Just base) -> Right <$> c_PQPutf param fmt base
        case esuccess of
          Left msg      -> return . Left $ msg
          Right success -> if success == 0
            then Left <$> (peekCString =<< c_PQgeterror)
            else putValues rest param fmt

-- VARIABLE-LENGTH CHARACTER TYPES

instance ToSQL BS.ByteString CString where
  pqFormatPut _ = "%text"
  toSQL _ bs conv = BS.useAsCString bs $ \cs -> conv . Right . Just $ cs

instance ToSQL Text CString where
  pqFormatPut _ = "%text"
  toSQL conn = toSQL conn . encodeUtf8

instance ToSQL String CString where
  pqFormatPut _ = "%text"
  toSQL _ s conv = withCString s $ \cs -> conv . Right . Just $ cs
