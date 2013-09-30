{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, Rank2Types, TupleSections #-}
module Database.PostgreSQL.PQTypes.Internal.SQL (
    SQL
  , mkSQL
  , value
  , (<+>)
  , (<?>)
  , withSQL
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Data.Monoid
import Data.String
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.Internal.C.Put
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.ToSQL

data SqlChunk where
  SqlString :: !BS.ByteString -> SqlChunk
  SqlValue  :: forall t. (Show t, ToSQL t) => !t -> SqlChunk

newtype SQL = SQL ([SqlChunk] -> [SqlChunk])

unSQL :: SQL -> [SqlChunk]
unSQL (SQL dlist) = dlist []

instance IsString SQL where
  fromString = mkSQL . T.encodeUtf8 . T.pack

instance Show SQL where
  showsPrec n = showsPrec n . concatMap conv . unSQL
    where
      conv (SqlString s) = BS.unpack s
      conv (SqlValue v) = "<" ++ show v ++ ">"

instance Monoid SQL where
  mempty = SQL id
  SQL a `mappend` SQL b = SQL (a . b)

mkSQL :: BS.ByteString -> SQL
mkSQL = SQL . (:) . SqlString

value :: (Show t, ToSQL t) => t -> SQL
value = SQL . (:) . SqlValue

(<+>) :: SQL -> SQL -> SQL
a <+> b = mconcat [a, mkSQL (BS.pack " "), b]
infixr 6 <+>

(<?>) :: (Show t, ToSQL t) => SQL -> t -> SQL
s <?> v = s <+> value v
infixr 7 <?>

----------------------------------------

withSQL :: SQL -> ParamAllocator -> (Ptr PGparam -> CString -> IO r) -> IO r
withSQL sql allocParam execute = alloca $ \err -> allocParam $ \param -> do
  nums <- newMVar (1::Int)
  query <- BS.concat <$> mapM (f param err nums) (unSQL sql)
  BS.useAsCString query (execute param)
  where
    f param err nums chunk = case chunk of
      SqlString s -> return s
      SqlValue v -> toSQL v allocParam $ \base -> BS.useAsCString (pqFormat v) $ \fmt -> do
        verifyPQTRes err "withSQL" =<< c_PQputf1 param err fmt base
        modifyMVar nums $ \n -> return . (, BS.pack $ "$" ++ show n) $! n+1
