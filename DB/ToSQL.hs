{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses
  , UndecidableInstances #-}
module DB.ToSQL (ToSQL(..)) where

import Data.Int
import Data.Text (Text)
import Data.Text.Encoding
import Foreign.C
import Foreign.Storable
import qualified Data.ByteString.Char8 as BS

class Storable base => ToSQL dest base | dest -> base where
  toSQL :: dest -> (Maybe (BS.ByteString, base) -> IO r) -> IO r

instance ToSQL dest base => ToSQL (Maybe dest) base where
  toSQL mdest conv = case mdest of
    Nothing   -> conv Nothing
    Just dest -> toSQL dest conv

instance ToSQL Int32 CInt where
  toSQL n conv = conv $ Just (BS.pack "%int4", fromIntegral n)

instance ToSQL BS.ByteString CString where
  toSQL bs conv = BS.useAsCString bs $ \cs -> conv $ Just (BS.pack "%text", cs)

instance ToSQL Text CString where
  toSQL = toSQL . encodeUtf8

instance ToSQL String CString where
  toSQL s conv = withCString s $ \cs -> conv $ Just (BS.pack "%text", cs)
