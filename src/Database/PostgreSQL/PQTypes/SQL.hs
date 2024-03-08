module Database.PostgreSQL.PQTypes.SQL
  ( SQL
  , mkSQL
  , sqlParam
  , (<?>)
  , isSqlEmpty
  ) where

import Control.Concurrent.MVar
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Foldable qualified as F
import Data.Monoid
import Data.Semigroup qualified as SG
import Data.Sequence qualified as S
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Foreign.Marshal.Alloc
import TextShow

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.Internal.C.Put
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.ToSQL

data SqlChunk where
  SqlString :: !T.Text -> SqlChunk
  SqlParam :: forall t. (Show t, ToSQL t) => !t -> SqlChunk

-- | Primary SQL type that supports efficient
-- concatenation and variable number of parameters.
newtype SQL = SQL (S.Seq SqlChunk)

unSQL :: SQL -> [SqlChunk]
unSQL (SQL chunks) = F.toList chunks

----------------------------------------

-- | Construct 'SQL' from 'String'.
instance IsString SQL where
  fromString = mkSQL . T.pack

instance IsSQL SQL where
  withSQL sql pa@(ParamAllocator allocParam) execute = do
    alloca $ \err -> allocParam $ \param -> do
      nums <- newMVar (1 :: Int)
      query <- T.concat <$> mapM (f param err nums) (unSQL sql)
      BS.useAsCString (T.encodeUtf8 query) (execute param)
    where
      f param err nums chunk = case chunk of
        SqlString s -> pure s
        SqlParam (v :: t) -> toSQL v pa $ \base ->
          BS.unsafeUseAsCString (pqFormat0 @t) $ \fmt -> do
            verifyPQTRes err "withSQL (SQL)" =<< c_PQputf1 param err fmt base
            modifyMVar nums $ \n -> pure . (,"$" <> showt n) $! n + 1

instance SG.Semigroup SQL where
  SQL a <> SQL b = SQL (a S.>< b)

instance Monoid SQL where
  mempty = mkSQL T.empty
  mappend = (SG.<>)

instance Show SQL where
  showsPrec n sql = ("SQL " ++) . (showsPrec n . concatMap conv . unSQL $ sql)
    where
      conv (SqlString s) = T.unpack s
      conv (SqlParam v) = "<" ++ show v ++ ">"

----------------------------------------

-- | Convert a 'Text' SQL string to the 'SQL' type.
mkSQL :: T.Text -> SQL
mkSQL = SQL . S.singleton . SqlString

-- | Embed parameter value inside 'SQL'.
sqlParam :: (Show t, ToSQL t) => t -> SQL
sqlParam = SQL . S.singleton . SqlParam

-- | Embed parameter value inside existing 'SQL'. Example:
--
-- > f :: Int32 -> String -> SQL
-- > f idx name = "SELECT foo FROM bar WHERE id =" <?> idx <+> "AND name =" <?> name
(<?>) :: (Show t, ToSQL t) => SQL -> t -> SQL
s <?> v = s <+> sqlParam v

infixr 7 <?>

----------------------------------------

-- | Test whether an 'SQL' is empty.
isSqlEmpty :: SQL -> Bool
isSqlEmpty (SQL chunks) = getAll $ F.foldMap (All . cmp) chunks
  where
    cmp (SqlString s) = s == T.empty
    cmp (SqlParam _) = False
