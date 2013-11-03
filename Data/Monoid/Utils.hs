{-# OPTIONS_GHC -Wall #-}
module Data.Monoid.Utils (
    mintercalate
  ) where

import Data.List
import Data.Monoid

mintercalate :: Monoid m => m -> [m] -> m
mintercalate m = mconcat . intersperse m
