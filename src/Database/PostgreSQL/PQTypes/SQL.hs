{-# LANGUAGE GADTs, Rank2Types, TupleSections #-}
module Database.PostgreSQL.PQTypes.SQL (
    SQL
  , mkSQL
  , sqlParam
  , (<?>)
  , isSqlEmpty
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Data.Monoid
import Data.String
import Foreign.Marshal.Alloc
import qualified Data.ByteString.Char8 as BS
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.Internal.C.Put
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.ToSQL

data SqlChunk where
  SqlString :: !BS.ByteString -> SqlChunk
  SqlParam  :: forall t. (Show t, ToSQL t) => !t -> SqlChunk

-- | Primary SQL type that supports efficient
-- concatenation and variable number of parameters.
newtype SQL = SQL (S.Seq SqlChunk)

unSQL :: SQL -> [SqlChunk]
unSQL (SQL chunks) = F.toList chunks

----------------------------------------

-- | Construct 'SQL' from 'String'. The underlying 'ByteString'
-- will be encoded as UTF-8, so if you are working with
-- a different encoding, you should not rely on this instance.
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
        SqlParam v -> toSQL v allocParam $ \base ->
          BS.useAsCString (pqFormat v) $ \fmt -> do
            verifyPQTRes err "withSQL (SQL)" =<< c_PQputf1 param err fmt base
            modifyMVar nums $ \n -> return . (, BS.pack $ "$" ++ show n) $! n+1

instance Monoid SQL where
  mempty = mkSQL BS.empty
  SQL a `mappend` SQL b = SQL (a S.>< b)

instance Show SQL where
  showsPrec n sql = ("SQL " ++) . (showsPrec n . concatMap conv . unSQL $ sql)
    where
      conv (SqlString s) = BS.unpack s
      conv (SqlParam v) = "<" ++ show v ++ ">"

----------------------------------------

-- | Convert 'ByteString' to 'SQL'.
mkSQL :: BS.ByteString -> SQL
mkSQL = SQL . S.singleton . SqlString

-- | Embed parameter value inside 'SQL'.
sqlParam :: (Show t, ToSQL t) => t -> SQL
sqlParam = SQL . S.singleton . SqlParam

-- | Embed parameter value inside existing 'SQL'. Example:
--
-- > f :: Int32 -> String -> SQL
-- > f idx name = "SELECT foo FROM bar WHERE id =" <?> idx <+> "AND name =" <?> name
--
(<?>) :: (Show t, ToSQL t) => SQL -> t -> SQL
s <?> v = s <+> sqlParam v
infixr 7 <?>

----------------------------------------

-- | Test whether an 'SQL' is empty.
isSqlEmpty :: SQL -> Bool
isSqlEmpty (SQL chunks) = getAll $ F.foldMap (All . cmp) chunks
  where
    cmp (SqlString s) = s == BS.empty
    cmp (SqlParam _)  = False
