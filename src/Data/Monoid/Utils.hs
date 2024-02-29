module Data.Monoid.Utils
  ( mintercalate
  , mspace
  , smappend
  , smconcat
  , (<+>)
  ) where

import Data.List
import Data.String

-- | Generalization of 'intercalate' to arbitrary 'Monoid'.
mintercalate :: Monoid m => m -> [m] -> m
mintercalate m = mconcat . intersperse m

-- | Generalization of separator to arbitrary 'Monoid'.
mspace :: (IsString m, Monoid m) => m
mspace = fromString " "

-- | Concatenate two elements with separator between them.
smappend :: (IsString m, Monoid m) => m -> m -> m
smappend a b = mconcat [a, mspace, b]

-- | Concatenate a list of elements, inserting separators between them.
smconcat :: (IsString m, Monoid m) => [m] -> m
smconcat = mintercalate mspace

-- | Infix version of 'smappend'.
(<+>) :: (IsString m, Monoid m) => m -> m -> m
(<+>) = smappend

infixr 6 <+>
