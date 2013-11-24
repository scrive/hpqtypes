{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, ScopedTypeVariables #-}
module Database.PostgreSQL.PQTypes.Single (
    Single(..)
  , unSingle
  ) where

import Data.Typeable

import Database.PostgreSQL.PQTypes.Format

-- | Representation of \"single element tuple\", used
-- to avoid resorting to usage of OverlappingInstances.
newtype Single a = Single a
  deriving (Eq, Functor, Ord, Show, Typeable)

-- | Extract underlying value.
unSingle :: Single a -> a
unSingle (Single a) = a

instance PQFormat t => PQFormat (Single t) where
  pqFormat _ = pqFormat (undefined::t)
