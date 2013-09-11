{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
module Database.PostgreSQL.Internal.SQL where

import Data.Monoid
import Data.String
import qualified Data.DList as D

import Database.PostgreSQL.Internal.C.Put
import Database.PostgreSQL.ToSQL

data SqlChunk
  = SCString String
  | forall dest base. (Show dest, PQPut base, ToSQL dest base) => SCValue dest

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
  SQL a `mappend` SQL b = SQL (a <> b)

value :: (Show dest, PQPut base, ToSQL dest base) => dest -> SQL
value = SQL . D.singleton . SCValue

(<?>) :: (Show dest, PQPut base, ToSQL dest base) => SQL -> dest -> SQL
s <?> v = s <> value v
infixl 7 <?>
