module Database.PostgreSQL.PQTypes.SQL
  ( SQL
  , mkSQL
  , sqlParam
  , (<?>)
  , isSqlEmpty
  ) where

import Data.ByteString.Char8 qualified as BS
import Data.Foldable qualified as F
import Data.List qualified as L
import Data.Monoid
import Data.Semigroup qualified as SG
import Data.Sequence qualified as S
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import TextShow

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.ToSQL

data SqlChunk where
  SqlString :: !T.Text -> SqlChunk
  SqlParam :: forall a. (Show a, ToSQL a) => !a -> SqlChunk

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
  withSQL sql execute =
    BS.useAsCString (T.encodeUtf8 query) $ \cquery ->
      execute cquery (reverse params)
    where
      query = T.concat chunks
      ((_, params), chunks) = L.mapAccumL f (1 :: Int, []) $ unSQL sql
      f acc@(n, ps) = \case
        SqlString s -> (acc, s)
        SqlParam v -> ((n + 1, toPQParam v : ps), "$" <> showt n)

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
sqlParam :: (Show a, ToSQL a) => a -> SQL
sqlParam = SQL . S.singleton . SqlParam

-- | Embed parameter value inside existing 'SQL'. Example:
--
-- > f :: Int32 -> String -> SQL
-- > f idx name = "SELECT foo FROM bar WHERE id =" <?> idx <+> "AND name =" <?> name
(<?>) :: (Show a, ToSQL a) => SQL -> a -> SQL
s <?> v = s <+> sqlParam v

infixr 7 <?>

----------------------------------------

-- | Test whether an 'SQL' is empty.
isSqlEmpty :: SQL -> Bool
isSqlEmpty (SQL chunks) = getAll $ F.foldMap (All . cmp) chunks
  where
    cmp (SqlString s) = s == T.empty
    cmp (SqlParam _) = False
