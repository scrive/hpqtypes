{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.Types where

import Data.ByteString (ByteString)

newtype Array a = Array [a]
  deriving (Eq, Ord, Show)

unArray :: Array a -> [a]
unArray (Array arr) = arr

newtype Binary = Binary ByteString
  deriving (Eq, Ord, Show)

unBinary :: Binary -> ByteString
unBinary (Binary bs) = bs
