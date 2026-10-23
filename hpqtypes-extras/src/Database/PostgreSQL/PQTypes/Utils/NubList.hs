module Database.PostgreSQL.PQTypes.Utils.NubList
  ( NubList -- opaque
  , toNubList -- smart construtor
  , fromNubList
  , overNubList
  ) where

import Data.Semigroup qualified as SG
import Data.Set qualified as Set
import Text.Read qualified as R

{-
  This module is a copy-paste fork of Distribution.Utils.NubList in Cabal
  (Cabal-2.0.1.1 as it happens) to avoid depending on the whole of the Cabal
  library. `NubListR` was removed in the process and `ordNubBy` and `listUnion`
  hand-inlined to avoid depending on more Cabal-specific modules.
-}

-- | NubList : A de-duplicated list that maintains the original order.
newtype NubList a
  = NubList {fromNubList :: [a]}
  deriving (Eq)

-- NubList assumes that nub retains the list order while removing duplicate
-- elements (keeping the first occurence). Documentation for "Data.List.nub"
-- does not specifically state that ordering is maintained so we will add a test
-- for that to the test suite.

-- | Smart constructor for the NubList type.
toNubList :: Ord a => [a] -> NubList a
toNubList = NubList . ordNubBy id

-- | Lift a function over lists to a function over NubLists.
overNubList :: Ord a => ([a] -> [a]) -> NubList a -> NubList a
overNubList f (NubList list) = toNubList . f $ list

instance Ord a => SG.Semigroup (NubList a) where
  (NubList xs) <> (NubList ys) = NubList $ xs `listUnion` ys
    where
      listUnion :: Ord a => [a] -> [a] -> [a]
      listUnion a b =
        a
          ++ ordNubBy id (filter (`Set.notMember` Set.fromList a) b)

instance Ord a => Monoid (NubList a) where
  mempty = NubList []
  mappend = (SG.<>)

instance Show a => Show (NubList a) where
  show (NubList list) = show list

instance (Ord a, Read a) => Read (NubList a) where
  readPrec = readNubList toNubList

-- | Helper used by NubList/NubListR's Read instances.
readNubList :: Read a => ([a] -> l a) -> R.ReadPrec (l a)
readNubList toList = R.parens . R.prec 10 $ fmap toList R.readPrec

ordNubBy :: Ord b => (a -> b) -> [a] -> [a]
ordNubBy f = go Set.empty
  where
    go !_ [] = []
    go !s (x : xs)
      | y `Set.member` s = go s xs
      | otherwise =
          let !s' = Set.insert y s
          in x : go s' xs
      where
        y = f x
