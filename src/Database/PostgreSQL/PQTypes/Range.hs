-- | Representation of the PostgreSQL range types.
module Database.PostgreSQL.PQTypes.Range
  ( Range (..)
  , Bound (..)
  ) where

-- | A range of values of type @a@, either empty or delimited by a pair of
-- bounds.
--
-- Note that the server normalizes ranges over discrete types, so a fetched
-- value doesn't necessarily have the bounds it was created with, e.g.
-- @int4range(1, 5, \'(]\')@ comes back as @v'Range' ('Incl' 2) ('Excl' 6)@.
data Range a
  = Empty
  | Range !(Bound a) !(Bound a)
  deriving stock (Eq, Functor, Ord, Show)

-- | One end of a t'Range': inclusive, exclusive or unbounded.
data Bound a
  = Incl !a
  | Excl !a
  | Inf
  deriving stock (Eq, Functor, Ord, Show)
