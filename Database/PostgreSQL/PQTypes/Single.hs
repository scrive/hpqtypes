{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, ScopedTypeVariables #-}
module Database.PostgreSQL.PQTypes.Single (
    Single(..)
  , unSingle
  ) where

import Data.Typeable

import Database.PostgreSQL.PQTypes.Format

newtype Single a = Single a
  deriving (Eq, Functor, Ord, Show, Typeable)

unSingle :: Single a -> a
unSingle (Single a) = a

instance PQFormat t => PQFormat (Single t) where
  pqFormat _ = pqFormat (undefined::t)
