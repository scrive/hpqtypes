{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Monoid.Space (
    SpaceMonoid(..)
  , smappend
  , smconcat
  , (<+>)
  ) where

import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString as BS

import Data.Monoid.Utils

class Monoid m => SpaceMonoid m where
  mspace :: m

instance SpaceMonoid String where
  mspace = " "

instance SpaceMonoid BS.ByteString where
  mspace = BS.singleton 32

instance SpaceMonoid T.Text where
  mspace = T.singleton ' '

----------------------------------------

smappend :: SpaceMonoid m => m -> m -> m
smappend a b = mconcat [a, mspace, b]

smconcat :: SpaceMonoid m => [m] -> m
smconcat = mintercalate mspace

(<+>) :: SpaceMonoid m => m -> m -> m
(<+>) = smappend
infixr 6 <+>
