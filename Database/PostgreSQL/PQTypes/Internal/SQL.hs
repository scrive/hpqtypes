{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
module Database.PostgreSQL.PQTypes.Internal.SQL (
    SQL(..)
  , SqlChunk(..)
  , unSQL
  , mkSQL
  , value
  , (<+>)
  , (<?>)
  ) where

import Data.Monoid
import Data.String
import qualified Data.ByteString.Char8 as BS
import qualified Data.DList as D
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Database.PostgreSQL.PQTypes.ToSQL

data SqlChunk
  = SCString !BS.ByteString
  | forall t. (Show t, ToSQL t) => SCValue !t

newtype SQL = SQL (D.DList SqlChunk)

unSQL :: SQL -> [SqlChunk]
unSQL (SQL dlist) = D.toList dlist

instance IsString SQL where
  fromString = mkSQL . T.encodeUtf8 . T.pack

instance Show SQL where
  show = show . concatMap conv . unSQL
    where
      conv (SCString s) = BS.unpack s
      conv (SCValue v) = "<" ++ show v ++ ">"

instance Monoid SQL where
  mempty = SQL D.empty
  SQL a `mappend` SQL b = SQL (a `mappend` b)

mkSQL :: BS.ByteString -> SQL
mkSQL = SQL . D.singleton . SCString

value :: (Show t, ToSQL t) => t -> SQL
value = SQL . D.singleton . SCValue

(<+>) :: SQL -> SQL -> SQL
a <+> b = mconcat [a, fromString " ", b]
infixr 6 <+>

(<?>) :: (Show t, ToSQL t) => SQL -> t -> SQL
s <?> v = s <+> value v
infixr 7 <?>
