module Database.PostgreSQL.PQTypes.Enum
  ( -- * Encoder class
    EnumEncodingAs (..)

    -- * Helpers, to be used with @deriving via@ (@-XDerivingVia@).
  , SQLEnum (..)
  , SQLEnumAsText (..)

    -- * For use in doctests.
  , isInjective
  ) where

import Control.Monad.Catch
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Typeable

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Oid
import Database.PostgreSQL.PQTypes.ToSQL

-- | Encoding of an enum @a@ as a value of a base type @base@.
--
-- Used by 'SQLEnum' and 'SQLEnumAsText' to derive SQL instances.
class (Ord base, Bounded a, Enum a) => EnumEncodingAs base a | a -> base where
  -- | Encode @a@ as the base type.
  encodeEnumAs :: a -> base

  -- | Include the inverse map as a top-level part of the 'EnumEncodingAs'
  -- instance to ensure it is only computed once by GHC.
  enumEncodingMap :: M.Map base a
  enumEncodingMap = M.fromList [(encodeEnumAs a, a) | a <- enumerate]

----------------------------------------

-- | Helper newtype to be used with @deriving via@ to derive @(PQFormat, ToSQL,
-- FromSQL)@ instances for enums, given an instance of 'EnumEncodingAs'.
--
-- /Hint:/ non-trivial 'Enum' instances can be derived using the 'generic-data'
-- package!
--
-- >>> :{
-- data Colours = Blue | Black | Red | Mauve | Orange
--   deriving (Eq, Show, Enum, Bounded)
-- instance EnumEncodingAs Int16 Colours where
--   encodeEnumAs = \case
--     Blue   -> 1
--     Black  -> 7
--     Red    -> 2
--     Mauve  -> 6
--     Orange -> 3
-- :}
--
-- /Note:/ To get SQL-specific instances use @DerivingVia@:
--
-- @
-- data Colours = ...
--   ...
--   deriving (PQFormat, ToSQL, FromSQL) via SQLEnum Colours
-- @
--
-- >>> isInjective (encodeEnumAs @Int16 @Colours)
-- True
newtype SQLEnum a = SQLEnum a

instance (EnumEncodingAs base a, PQFormat base) => PQFormat (SQLEnum a) where
  pqOid = pqOid @base
  pqArrayOid = pqArrayOid @base

instance (EnumEncodingAs base a, ToSQL base) => ToSQL (SQLEnum a) where
  toSQL (SQLEnum a) = toSQL $ encodeEnumAs a

instance
  ( EnumEncodingAs base a
  , Enum base
  , FromSQL base
  , Show base
  , Typeable base
  )
  => FromSQL (SQLEnum a)
  where
  fromSQL = do
    b <- fromSQL
    case M.lookup b $ enumEncodingMap @base @a of
      Nothing ->
        throwM
          RangeError
            { reRange = foldr mkIntervals [] . M.keys $ enumEncodingMap @base @a
            , reValue = b
            }
      Just a -> pure $ SQLEnum a
    where
      -- Decompose keys of the map (ascending and duplicate-free) into a list of
      -- intervals.
      mkIntervals :: base -> [(base, base)] -> [(base, base)]
      mkIntervals a = \case
        [] -> [(a, a)]
        ivs@((lower, upper) : rest)
          | succ a == lower -> (a, upper) : rest
          | otherwise -> (a, a) : ivs

----------------------------------------

-- | A variant of 'SQLEnum' for enums encoded as text, in particular as values
-- of actual PostgreSQL enum types ('SQLEnum' can't be used, because decoding
-- requires an 'Enum' instance of the base type, which 'Text' doesn't have).
--
-- Encoded values are sent with their type unspecified, so that the server
-- infers it from the context, which makes them usable directly against both
-- text and enum columns. The flip side is that they cannot be used in
-- contexts that provide no type information (e.g. @SELECT $1@ alone fails
-- with \"could not determine data type of parameter\").
--
-- Lists and 'Data.Vector.Vector's of such values are sent as @text[]@ (a
-- binary array parameter carries a concrete element type, so it cannot be
-- left for the server to infer): they work directly against @text@ columns,
-- while usage against columns of an actual enum type requires a cast, e.g.
-- @... = ANY($1::my_enum_type[])@ (the cast is a genuine conversion, since
-- the parameter is a @text[]@ value).
--
-- Note that decoding is different from @'SQLEnum' a@ with an
-- @'EnumEncodingAs' 'Text' a@ instance: values are decoded with 'decodeEnum',
-- which accepts both text and PostgreSQL enum types, and errors are reported
-- as 'InvalidValue' with the list of valid labels, not 'RangeError'.
--
-- >>> :{
-- data Person = Alfred | Bertrand | Charles
--   deriving (Eq, Show, Enum, Bounded)
-- instance EnumEncodingAs Text Person where
--   encodeEnumAs = \case
--     Alfred   -> "alfred"
--     Bertrand -> "bertrand"
--     Charles  -> "charles"
-- :}
--
-- /Note:/ To get SQL-specific instances use @DerivingVia@:
--
-- @
-- data Person = ...
--   ...
--   deriving (PQFormat, ToSQL, FromSQL) via SQLEnumAsText Person
-- @
--
-- >>> isInjective (encodeEnumAs @Text @Person)
-- True
newtype SQLEnumAsText a = SQLEnumAsText a

instance EnumEncodingAs Text a => PQFormat (SQLEnumAsText a) where
  pqOid = unspecifiedOid
  pqArrayOid = textArrayOid

instance EnumEncodingAs Text a => ToSQL (SQLEnumAsText a) where
  toSQL (SQLEnumAsText a) = toSQL $ encodeEnumAs a
  arrayElemOid = textOid

instance EnumEncodingAs Text a => FromSQL (SQLEnumAsText a) where
  fromSQL = SQLEnumAsText <$> decodeEnum (enumEncodingMap @Text @a)

----------------------------------------
-- Helpers

-- | To be used in doctests to prove injectivity of encoding functions.
--
-- >>> isInjective (id :: Bool -> Bool)
-- True
--
-- >>> isInjective (\(_ :: Bool) -> False)
-- False
isInjective :: (Enum a, Bounded a, Eq a, Eq b) => (a -> b) -> Bool
isInjective f = null [(a, b) | a <- enumerate, b <- enumerate, a /= b, f a == f b]

-- | Internal helper: all values of a finitely enumerable type.
enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

-- $setup
-- >>> import Data.Int
