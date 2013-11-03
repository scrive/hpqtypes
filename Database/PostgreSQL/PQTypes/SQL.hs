{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, Rank2Types, TupleSections #-}
module Database.PostgreSQL.PQTypes.SQL (
    SQL
  , mkSQL
  , value
  , (<?>)
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Data.Monoid
import Data.String
import Foreign.Marshal.Alloc
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Monoid.Space
import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.Internal.C.Put
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.ToSQL

data SqlChunk where
  SqlString :: !BS.ByteString -> SqlChunk
  SqlValue  :: forall t. (Show t, ToSQL t) => !t -> SqlChunk

newtype SQL = SQL ([SqlChunk] -> [SqlChunk])

unSQL :: SQL -> [SqlChunk]
unSQL (SQL dlist) = dlist []

----------------------------------------

instance IsString SQL where
  fromString = mkSQL . T.encodeUtf8 . T.pack

instance IsSQL SQL where
  someSQL = SomeSQL
  withSQL sql allocParam execute = alloca $ \err -> allocParam $ \param -> do
    nums <- newMVar (1::Int)
    query <- BS.concat <$> mapM (f param err nums) (unSQL sql)
    BS.useAsCString query (execute param)
    where
      f param err nums chunk = case chunk of
        SqlString s -> return s
        SqlValue v -> toSQL v allocParam $ \base ->
          BS.useAsCString (pqFormat v) $ \fmt -> do
            verifyPQTRes err "withSQL (SQL)" =<< c_PQputf1 param err fmt base
            modifyMVar nums $ \n -> return . (, BS.pack $ "$" ++ show n) $! n+1

instance Monoid SQL where
  mempty = SQL id
  SQL a `mappend` SQL b = SQL (a . b)

instance SpaceMonoid SQL where
  mspace = mkSQL mspace

instance Show SQL where
  showsPrec n sql = ("SQL " ++) . (showsPrec n . concatMap conv . unSQL $ sql)
    where
      conv (SqlString s) = BS.unpack s
      conv (SqlValue v) = "<" ++ show v ++ ">"

----------------------------------------

mkSQL :: BS.ByteString -> SQL
mkSQL = SQL . (:) . SqlString

value :: (Show t, ToSQL t) => t -> SQL
value = SQL . (:) . SqlValue

(<?>) :: (Show t, ToSQL t) => SQL -> t -> SQL
s <?> v = s <+> value v
infixr 7 <?>
