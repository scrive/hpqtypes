# Migrating from 1.x to 2.0

Version 2.0 removed the bundled `libpqtypes` C library. Queries now run over
plain `libpq`. Encoders and decoders derived from the `postgresql-binary`
package convert values to and from the binary wire format. This change
affected the decoding API and the shape of `ToSQL` and `FromSQL` instances.
Most call sites migrate mechanically. This guide describes the recurring
patterns. The
[changelog](https://github.com/scrive/hpqtypes/blob/master/CHANGELOG.md)
lists all changes.

## Row fetching takes a decoder, not a projection

The `FromRow` class is gone. The library no longer infers the type of a row
from a projection function. Instead, `fetchMany`, `fetchMaybe`, `fetchOne`,
`foldrDB`, `foldlDB` and `mapDB_` take a `RowDecoder`. You build a
`RowDecoder` from the `fromSQL` decoders of consecutive columns:

```haskell
-- 1.x
runSQL_ "SELECT id, name FROM authors_ ORDER BY name"
mapDB_ $ \(aid :: Int64, name :: String) ->
  printLn $ show aid <> ": " <> name

-- 2.0
runSQL_ "SELECT id, name FROM authors_ ORDER BY name"
mapDB_ ((,) <$> fromSQL @Int64 <*> fromSQL @String) $ \(aid, name) ->
  printLn $ show aid <> ": " <> name
```

To build a record, apply the constructor instead of a tuple
(`Thing <$> fromSQL <*> fromSQL <*> fromSQL`). The `genericDecoder` function
decodes any product type with a `Generic` instance, tuples included.

For a single column, `runIdentity` disappears:

```haskell
-- 1.x
tid :: Int64 <- fetchOne runIdentity

-- 2.0
tid <- fetchOne $ fromSQL @Int64
```

`RowDecoder` is also a monad. A decoder of a wide row can bind one field per
line instead of nested applicative operators. It can also inspect the fields
that it already decoded.

`QueryResult` no longer carries a row type. It lost its `Functor` and
`Foldable` instances. If your code decoded rows by traversing `queryResult`,
use the fetch functions instead. The fetch functions no longer materialize
all rows up front. They decode each row right before the fold function
consumes it. A fold over a large result runs in constant memory.

## Query parameters

`rawSQL` and `<?>` work as before, with one exception. A parameter tuple can
have at most 10 fields, down from 50. To join a wider parameter list from
smaller tuples, use `:++:`, the new name of `:*:`:

```haskell
runQuery_ $ rawSQL "INSERT ... VALUES ($1, ..., $12)" $
  (p1, p2, p3, p4, p5, p6) :++: (p7, p8, p9, p10, p11, p12)
```

## Arrays are plain lists

The `Database.PostgreSQL.PQTypes.Array` module is gone, together with its
`Array1`, `Array2`, `CompositeArray1` and `CompositeArray2` wrappers. Lists
and `Vector`s have `ToSQL` and `FromSQL` instances directly. A
multi-dimensional array is a nested list or `Vector`. Remove the wrappers
from both parameters and decoders:

```haskell
-- 1.x
runQuery_ $ "DELETE FROM jobs_ WHERE id = ANY(" <?> Array1 ids <+> ")"
Array1 values <- fetchOne runIdentity

-- 2.0
runQuery_ $ "DELETE FROM jobs_ WHERE id = ANY(" <?> ids <+> ")"
values <- fetchOne $ fromSQL @[T.Text]
```

## Composite types

Registration of composite types is gone. `ConnectionSettings` has no
`csComposites` field anymore. Delete any code that passes composite type
names around only to register them.

On the decoding side, the `decodeComposite` combinator replaces the
`CompositeRow` and `CompositeFromSQL` machinery. It wraps a `RowDecoder` of
the fields, typically `genericDecoder`:

```haskell
-- 1.x
type instance CompositeRow Book = (Int64, String, Int32)

instance PQFormat Book where
  pqFormat = "%book_"

instance CompositeFromSQL Book where
  toComposite (bid, name, year) = Book bid name year

-- 2.0
data Book = Book
  { bookID :: Int64
  , bookName :: String
  , bookYear :: Int32
  }
  deriving stock (Generic)

instance FromSQL Book where
  fromSQL = decodeComposite genericDecoder
```

Anonymous row values now decode fine. If a composite type existed only to
ship nested data to the client, delete both the type and the cast to it:

```haskell
-- 1.x (requires CREATE TYPE book_ and csComposites = ["book_"])
runSQL_ "SELECT a.name, ARRAY(SELECT (b.id, b.name, b.year)::book_ FROM books_ b WHERE b.author_id = a.id) FROM authors_ a"
mapDB_ $ \(author :: String, CompositeArray1 (books :: [Book])) -> ...

-- 2.0
runSQL_ "SELECT a.name, ARRAY(SELECT (b.id, b.name, b.year) FROM books_ b WHERE b.author_id = a.id) FROM authors_ a"
mapDB_ ((,) <$> fromSQL @String <*> fromSQL @[Book]) $ \(author, books) -> ...
```

The library no longer supports *encoding* of composites (`CompositeToSQL`).
Send the fields as separate parameters and assemble the row in SQL, e.g.
`($1, $2, $3)::book_`.

## `ToSQL`/`FromSQL` instances

The `PQBase` and `PQDest` type families are gone. `toSQL` produces a
`Maybe Encoding` in the binary wire format, with `Nothing` for NULL.
`fromSQL` *is* a `RowDecoder`, with no argument to thread through. An
instance that wraps another type keeps its shape minus the boilerplate. To
reject an invalid value, use the decoder's `MonadThrow` instance:

```haskell
-- 1.x
instance PQFormat Host where
  pqFormat = pqFormat @T.Text

instance ToSQL Host where
  type PQDest Host = PQDest T.Text
  toSQL = toSQL . unHost

instance FromSQL Host where
  type PQBase Host = PQBase T.Text
  fromSQL mbase = fromSQL mbase >>= either throwM pure . parseHost

-- 2.0
instance PQFormat Host where
  pqFormat = pqFormat @T.Text

instance ToSQL Host where
  toSQL = toSQL . unHost

instance FromSQL Host where
  fromSQL = either throwM pure . parseHost =<< fromSQL
```

`PQFormat` now identifies the PostgreSQL type by its Oid instead of a format
string. In most cases you can reuse the instance of a type with the same
representation, as above, or with `deriving PQFormat via T.Text`. `PQFormat`
is also no longer a superclass of `FromSQL`. A type that you only fetch
needs no `PQFormat` instance at all. To decode a scalar type from scratch,
pair the `decodeScalar` combinator with a `Value` parser from
`Database.PostgreSQL.PQTypes.Internal.Decoding`.

Newtype deriving keeps working. No type families stand in its way anymore:

```haskell
newtype ConsumerID = ConsumerID Int64
  deriving newtype (PQFormat, ToSQL, FromSQL)
```

## JSON

`JSON` and `JSONB` are no longer limited to `Value` and JSON text. They
serialize and deserialize the wrapped type with its `ToJSON` and `FromJSON`
instances. This makes them `deriving via` helpers and retires `aesonFromSQL`
and `aesonToSQL`:

```haskell
-- 1.x
data Config = ...
  deriving anyclass (FromJSON, ToJSON)

instance PQFormat Config where
  pqFormat = pqFormat @(JSONB Value)

instance ToSQL Config where
  type PQDest Config = PQDest (JSONB Value)
  toSQL = aesonToSQL

instance FromSQL Config where
  type PQBase Config = PQBase (JSONB Value)
  fromSQL = aesonFromSQL

-- 2.0
data Config = ...
  deriving anyclass (FromJSON, ToJSON)
  deriving (PQFormat, ToSQL, FromSQL) via JSONB Config
```

The `RawJSON` and `RawJSONB` types hold the instances that pass JSON text
through unparsed. `JSON` or `JSONB` of a `ByteString` becomes `RawJSON` or
`RawJSONB` of a strict `ByteString`. The lazy instances are gone. The
`encodeRawJSON`, `decodeRawJSON` and `eitherDecodeRawJSON` functions and
their `JSONB` twins convert between a raw value and a type with aeson
instances.

## Enums

The `deriving via` helpers for enum types moved from `hpqtypes-extras`
(`Database.PostgreSQL.PQTypes.Deriving`) into `hpqtypes` itself. They live
in the `Database.PostgreSQL.PQTypes.Enum` module, which
`Database.PostgreSQL.PQTypes` re-exports. The `EnumEncoding` class with its
`EnumBase` type family became `EnumEncodingAs`, with the base type as a class
parameter. `EnumEncodingAs` now also backs `SQLEnumAsText` and replaces
`EnumAsTextEncoding`:

```haskell
-- 1.x (hpqtypes-extras)
instance EnumEncoding Lang where
  type EnumBase Lang = Int16
  encodeEnum = \case
    LangEN -> 1
    LangSV -> 2

-- 2.0
instance EnumEncodingAs Int16 Lang where
  encodeEnumAs = \case
    LangEN -> 1
    LangSV -> 2
```

The deriving clause itself
(`deriving (PQFormat, ToSQL, FromSQL) via SQLEnum Lang`) stays as it was.

## Smaller things to watch for

* `QueryError` is gone. A failure that produces no `PGresult`, e.g. a
  connection that dies mid-query, throws `LibPQError`. Adjust exception
  handlers that match `QueryError`.
* When the library decodes a row, it compares the width of the result with
  the decoder. Before that, it does not. A mismatched fetch from an empty
  result no longer throws.
* You can still send `Int` and `Word` as parameters, but you can't fetch
  them, because their size depends on the architecture. Fetch `Int64` or
  `Word64` instead.
* `Interval` is now opaque and mirrors the wire format. Build a value with
  `iyears`, `imonths`, `idays`, `ihours`, `iminutes`, `iseconds` and
  `imicroseconds`, and combine the parts with `<>`.
* The decoder rejects the `infinity` and `-infinity` values of the date and
  time types. In 1.x they silently mapped to bogus finite values.
* Execution of a `COPY` statement now throws instead of corrupting the
  connection state.
* The library requires GHC 9.6 or newer.
