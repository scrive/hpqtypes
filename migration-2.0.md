# Migrating from 1.x to 2.0

In version 2.0 the bundled `libpqtypes` C library is gone: queries now run
over plain `libpq` and values are converted to and from the binary wire
format with encoders and decoders derived from the `postgresql-binary`
package. This changed the decoding API and the shape of `ToSQL`/`FromSQL`
instances; most call sites migrate mechanically. The recurring patterns are
described below, the complete list of changes is in the
[changelog](https://github.com/scrive/hpqtypes/blob/master/CHANGELOG.md).

## Row fetching takes a decoder, not a projection

The `FromRow` class is gone and the type of each row is no longer inferred
from a projection function. Instead, `fetchMany`, `fetchMaybe`, `fetchOne`,
`foldrDB`, `foldlDB` and `mapDB_` take a `RowDecoder`, built by combining
`fromSQL` decoders of consecutive columns:

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

Records are built by applying the constructor instead of pairing
(`Thing <$> fromSQL <*> fromSQL <*> fromSQL`), and `genericDecoder` decodes
any product type with a `Generic` instance, tuples included.

For a single column, `runIdentity` disappears:

```haskell
-- 1.x
tid :: Int64 <- fetchOne runIdentity

-- 2.0
tid <- fetchOne $ fromSQL @Int64
```

`RowDecoder` is also a monad, so a decoder of a wide row can bind one field
per line instead of nesting applicative operators, and inspect already
decoded fields while doing so.

`QueryResult` no longer carries a row type and lost its `Functor` and
`Foldable` instances; code that decoded rows by traversing `queryResult`
should use the fetch functions. Note that they no longer materialize all
rows up front — each row is decoded right before the fold function consumes
it, so folding over a large result runs in constant memory.

## Query parameters

`rawSQL` and `<?>` work as before, except a parameter tuple can have at
most 10 fields (down from 50). Wider parameter lists are joined from
smaller tuples with `:++:`, the new name of `:*:`:

```haskell
runQuery_ $ rawSQL "INSERT ... VALUES ($1, ..., $12)" $
  (p1, p2, p3, p4, p5, p6) :++: (p7, p8, p9, p10, p11, p12)
```

## Arrays are plain lists

The `Database.PostgreSQL.PQTypes.Array` module is gone together with its
`Array1`, `Array2`, `CompositeArray1` and `CompositeArray2` wrappers.
Lists and `Vector`s have `ToSQL`/`FromSQL` instances directly (nested for
multi-dimensional arrays), so the wrappers simply disappear from both
parameters and decoders:

```haskell
-- 1.x
runQuery_ $ "DELETE FROM jobs_ WHERE id = ANY(" <?> Array1 ids <+> ")"
Array1 values <- fetchOne runIdentity

-- 2.0
runQuery_ $ "DELETE FROM jobs_ WHERE id = ANY(" <?> ids <+> ")"
values <- fetchOne $ fromSQL @[T.Text]
```

## Composite types

Registration of composite types is gone: `ConnectionSettings` has no
`csComposites` field anymore and any code passing composite type names
around just to register them can be deleted.

On the decoding side, the `CompositeRow`/`CompositeFromSQL` machinery is
replaced by the `decodeComposite` combinator, which wraps a `RowDecoder`
of the fields — typically `genericDecoder`:

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

Moreover, anonymous row values now decode fine, so where a composite type
existed only to ship nested data to the client, both the type and the cast
to it can go:

```haskell
-- 1.x (requires CREATE TYPE book_ and csComposites = ["book_"])
runSQL_ "SELECT a.name, ARRAY(SELECT (b.id, b.name, b.year)::book_ FROM books_ b WHERE b.author_id = a.id) FROM authors_ a"
mapDB_ $ \(author :: String, CompositeArray1 (books :: [Book])) -> ...

-- 2.0
runSQL_ "SELECT a.name, ARRAY(SELECT (b.id, b.name, b.year) FROM books_ b WHERE b.author_id = a.id) FROM authors_ a"
mapDB_ ((,) <$> fromSQL @String <*> fromSQL @[Book]) $ \(author, books) -> ...
```

What's no longer supported is *encoding* of composites (`CompositeToSQL`):
send the fields as separate parameters and assemble the row in SQL, e.g.
`($1, $2, $3)::book_`.

## `ToSQL`/`FromSQL` instances

The `PQBase`/`PQDest` type families are gone: `toSQL` produces a
`Maybe Encoding` in the binary wire format (with `Nothing` for NULL) and
`fromSQL` *is* a `RowDecoder`, with no argument to thread through. An
instance wrapping another type keeps its shape minus the boilerplate, and
validation uses the decoder's `MonadThrow` instance:

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

`PQFormat` now identifies the PostgreSQL type by its Oid rather than a
format string; reusing the instance of a type with the same representation
(as above, or with `deriving PQFormat via T.Text`) is almost always enough.
It's also no longer a superclass of `FromSQL` — a type that is only ever
fetched needs no `PQFormat` instance at all. A scalar type decoded from
scratch pairs the `decodeScalar` combinator with a `Value` parser from
`Database.PostgreSQL.PQTypes.Internal.Decoding`.

Newtype deriving keeps working and no longer has type families standing in
its way:

```haskell
newtype ConsumerID = ConsumerID Int64
  deriving newtype (PQFormat, ToSQL, FromSQL)
```

## Enums

The `deriving via` helpers for enum types from `hpqtypes-extras`
(`Database.PostgreSQL.PQTypes.Deriving`) moved into `hpqtypes` itself (the
`Database.PostgreSQL.PQTypes.Enum` module, re-exported from
`Database.PostgreSQL.PQTypes`). The `EnumEncoding` class with its
`EnumBase` type family became `EnumEncodingAs` with the base type as a
class parameter, and it now backs `SQLEnumAsText` too, replacing
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

* `QueryError` is gone. Failures that produce no `PGresult` (e.g. the
  connection dying mid-query) throw `LibPQError`, so exception handlers
  matching `QueryError` need adjusting.
* The width of a result is checked against the decoder only when a row is
  actually decoded, so a mismatched fetch from an empty result no longer
  throws.
* `Int` and `Word` can still be sent as parameters, but not fetched — their
  size is architecture-dependent. Fetch `Int64`/`Word64` instead.
* `Interval` is now opaque, mirroring the wire format. Values are built
  with `iyears`, `imonths`, `idays`, `ihours`, `iminutes`, `iseconds` and
  `imicroseconds`, combined with `<>`.
* `infinity` and `-infinity` values of the date/time types are rejected at
  decode time instead of silently mapping to bogus finite values.
* Executing a `COPY` statement now throws instead of corrupting the
  connection state.
* GHC 9.6 or newer is required.
