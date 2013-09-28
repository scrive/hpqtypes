{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor, ScopedTypeVariables #-}
module Database.PostgreSQL.PQTypes.Single (
    Single(..)
  , unSingle
  ) where

import Database.PostgreSQL.PQTypes.Format

newtype Single a = Single a
  deriving (Eq, Functor, Ord, Show)

unSingle :: Single a -> a
unSingle (Single a) = a

instance PQFormat t => PQFormat (Single t) where
  pqFormat _ = pqFormat (undefined::t)
