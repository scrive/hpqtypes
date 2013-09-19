{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
module Database.PostgreSQL.PQTypes.Internal.SQL (
    SQL(..)
  , SqlChunk(..)
  , unSQL
  , value
  , (<+>)
  , (<?>)
  ) where

import Data.Monoid
import Data.String
import qualified Data.DList as D

import Database.PostgreSQL.PQTypes.ToSQL

data SqlChunk
  = SCString !String
  | forall t. (Show t, ToSQL t) => SCValue !t

newtype SQL = SQL (D.DList SqlChunk)

unSQL :: SQL -> [SqlChunk]
unSQL (SQL dlist) = D.toList dlist

instance IsString SQL where
  fromString = SQL . D.singleton . SCString

instance Show SQL where
  show = show . concatMap conv . unSQL
    where
      conv (SCString s) = s
      conv (SCValue v) = "<" ++ show v ++ ">"

instance Monoid SQL where
  mempty = SQL D.empty
  SQL a `mappend` SQL b = SQL (a `mappend` b)

value :: (Show t, ToSQL t) => t -> SQL
value = SQL . D.singleton . SCValue

(<+>) :: SQL -> SQL -> SQL
a <+> b = a `mappend` (fromString " " `mappend` b)
infixr 6 <+>

(<?>) :: (Show t, ToSQL t) => SQL -> t -> SQL
s <?> v = s <+> value v
infixr 7 <?>
