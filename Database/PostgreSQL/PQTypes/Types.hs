{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
module Database.PostgreSQL.PQTypes.Types where

newtype Array a = Array [a]
  deriving (Eq, Functor, Ord, Show)

unArray :: Array a -> [a]
unArray (Array arr) = arr

newtype Binary b = Binary b
  deriving (Eq, Functor, Ord, Show)

unBinary :: Binary b -> b
unBinary (Binary b) = b
