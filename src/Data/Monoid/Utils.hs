module Data.Monoid.Utils (
    mintercalate
  , mspace
  , smappend
  , smconcat
  , (<+>)
  ) where

import Data.List
import Data.Monoid
import Data.String
import Prelude

-- | Generalization of 'intercalate' to arbitrary 'Monoid'.
{-# INLINE mintercalate #-}
mintercalate :: Monoid m => m -> [m] -> m
mintercalate m = mconcat . intersperse m

-- | Generalization of separator to arbitrary 'Monoid'.
{-# INLINE mspace #-}
mspace :: (IsString m, Monoid m) => m
mspace = fromString " "

-- | Concatenate two elements with separator between them.
{-# INLINE smappend #-}
smappend :: (IsString m, Monoid m) => m -> m -> m
smappend a b = mconcat [a, mspace, b]

-- | Concatenate a list of elements, inserting separators between them.
{-# INLINE smconcat #-}
smconcat :: (IsString m, Monoid m) => [m] -> m
smconcat = mintercalate mspace

-- | Infix version of 'smappend'.
{-# INLINE (<+>) #-}
(<+>) :: (IsString m, Monoid m) => m -> m -> m
(<+>) = smappend
infixr 6 <+>
