{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
module Database.PostgreSQL.PQTypes.Types where

newtype Binary b = Binary b
  deriving (Eq, Functor, Ord, Show)

unBinary :: Binary b -> b
unBinary (Binary b) = b
