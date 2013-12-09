module Data.Monoid.Utils (
    mintercalate
  ) where

import Data.List
import Data.Monoid

-- | Generalization of 'intercalate' to arbitrary 'Monoid'.
mintercalate :: Monoid m => m -> [m] -> m
mintercalate m = mconcat . intersperse m
