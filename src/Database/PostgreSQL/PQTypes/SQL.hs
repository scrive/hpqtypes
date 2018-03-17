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
import Prelude
import TextShow
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Foldable as F
import qualified Data.Semigroup as SG
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
  SqlString :: !T.Text -> SqlChunk
  SqlParam  :: forall t. (Show t, ToSQL t) => !t -> SqlChunk

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
      nums <- newMVar (1::Int)
      query <- T.concat <$> mapM (f param err nums) (unSQL sql)
      BS.useAsCString (T.encodeUtf8 query) (execute param)
    where
      f param err nums chunk = case chunk of
        SqlString s -> return s
        SqlParam v -> toSQL v pa $ \base ->
          BS.unsafeUseAsCString (pqFormat0 v) $ \fmt -> do
            verifyPQTRes err "withSQL (SQL)" =<< c_PQputf1 param err fmt base
            modifyMVar nums $ \n -> return . (, "$" <> showt n) $! n+1

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

-- | Convert 'ByteString' to 'SQL'.
mkSQL :: T.Text -> SQL
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
    cmp (SqlString s) = s == T.empty
    cmp (SqlParam _)  = False
