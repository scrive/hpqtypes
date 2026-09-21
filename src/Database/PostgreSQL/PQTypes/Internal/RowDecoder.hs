{-# LANGUAGE NoFieldSelectors #-}

-- | Definition of the machinery for decoding query results.
module Database.PostgreSQL.PQTypes.Internal.RowDecoder
  ( -- * Row decoder
    RowDecoder
  , runDecoder

    -- ** Combinators
  , decodeArray
  , decodeComposite
  , decodeEnum
  , decodeNullable
  , decodeScalar
  , decodeScalarArray
  ) where

import BinaryParser qualified as BP
import Control.Exception qualified as E
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Foreign.C.Types
import Foreign.Ptr

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Decoding qualified as D
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Oid
import Database.PostgreSQL.PQTypes.Internal.Utils

type role RowDecoder representational

-- | Decoder of values of type @a@ from fields of a query result. It's a
-- monad, so you build a decoder of a compound value from the decoders of its
-- fields in sequence:
--
-- > data T = T Int32 T.Text Bool
-- >
-- > decodeT :: RowDecoder T
-- > decodeT = T <$> fromSQL <*> fromSQL <*> fromSQL
newtype RowDecoder a = RowDecoder (DecoderState -> IO (a, DecoderState))
  deriving
    ( Applicative
    , Functor
    , Monad
    )
    via StateT DecoderState IO

-- | 'throwM' wraps the exception in t'ConversionError' with the position of
-- the most recently consumed field, like the errors of the field decoders
-- themselves. Use it to report an error found after a field was decoded, e.g.
-- an invalid decoded value.
--
-- /Note:/ if the decoder throws before it consumed the field that the error
-- concerns, the position points at the preceding field. If the decoder
-- consumed no field yet, the error carries no position at all.
instance MonadThrow RowDecoder where
  throwM err = mkDecoder $ do
    DecoderState fs idx <- get
    lift $
      if idx == 0
        -- The decoder consumed no field yet, so there's no position to attach.
        then E.throwIO err
        else do
          -- idx is the index of the next field to consume.
          name <- fs.fieldName (idx - 1)
          E.throwIO
            ConversionError
              { convColumn = idx
              , convColumnName = name
              , convRow = fs.row + 1
              , convError = err
              }

instance MonadFail RowDecoder where
  fail = throwM . HPQTypesError

-- | Internal helper for defining decoders in terms of t'StateT'.
mkDecoder :: StateT DecoderState IO a -> RowDecoder a
mkDecoder = RowDecoder . runStateT

-- | State of the decoder: the source of fields along with the index of the
-- next field to decode. Only the index changes as decoding progresses.
data DecoderState = DecoderState !FieldSource !Int

----------------------------------------

-- | Source of fields for a decoder: columns of a row of a query result,
-- fields of a record value or elements of an array.
data FieldSource = FieldSource
  { numFields :: !Int
  , row :: !Int
  -- ^ Row of the query result the fields belong to, for error reporting.
  , getField :: !(Int -> IO Field)
  , isNull :: !(Int -> IO Bool)
  , fieldName :: !(Int -> IO String)
  }

-- | A field source backed by a function that produces fields. The strictness
-- annotations of t'FieldSource' force only the function closures, so the
-- source constructs a t'Field' only when a decoder asks for it.
mkFieldSource :: Int -> Int -> (Int -> Field) -> FieldSource
mkFieldSource srcRow n mkField =
  FieldSource
    { numFields = n
    , row = srcRow
    , getField = pure . mkField
    , isNull = \idx -> pure $ case mkField idx of
        Field _ Nothing -> True
        Field _ Just {} -> False
        SubArray {} -> False
    , fieldName = \_ -> pure ""
    }

----------------------------------------

-- | A field to be decoded.
data Field
  = -- | Type and value of the field ('Nothing' represents NULL).
    Field !Oid !(Maybe BS.ByteString)
  | -- | Sub-array of a multi-dimensional array (see 'decodeArray').
    SubArray !ArrayData

-- | Parsed representation of an array value.
data ArrayData = ArrayData
  { elemOid :: !Oid
  , dims :: ![Int]
  , elems :: !(V.Vector (Maybe BS.ByteString))
  -- ^ Elements of the array in row-major order.
  }

----------------------------------------
-- Running a decoder

-- | Run a decoder against a row of a query result. If the decoder didn't
-- consume all the columns, throw t'RowLengthMismatch'.
runDecoder :: RowDecoder a -> Ptr PGresult -> CInt -> IO a
runDecoder dec res i = do
  numFields <- fromIntegral <$> c_PQnfields res
  runDecoderWith dec $
    FieldSource
      { numFields = numFields
      , row = fromIntegral i
      , getField = \idx -> do
          let j = fromIntegral idx
          oid <- c_PQftype res j
          isnull <- c_PQgetisnull res i j
          mvalue <-
            if isnull == 1
              then pure Nothing
              else do
                ptr <- c_PQgetvalue res i j
                len <- c_PQgetlength res i j
                Just <$> BS.packCStringLen (ptr, fromIntegral len)
          pure $ Field oid mvalue
      , isNull = \idx -> (== 1) <$> c_PQgetisnull res i (fromIntegral idx)
      , fieldName = \idx -> safePeekCString' =<< c_PQfname res (fromIntegral idx)
      }

-- | Run a decoder against a field source. If the decoder didn't consume all
-- the fields, throw t'RowLengthMismatch'.
runDecoderWith :: RowDecoder a -> FieldSource -> IO a
runDecoderWith (RowDecoder dec) fs = do
  (a, DecoderState _ consumed) <- dec $ DecoderState fs 0
  when (consumed /= fs.numFields) . E.throwIO $
    RowLengthMismatch
      { lengthExpected = consumed
      , lengthDelivered = fs.numFields
      }
  pure a

----------------------------------------
-- Decoders

-- | Decode the next field with the given decoder. NULL decodes to 'Nothing'.
--
-- The given decoder doesn't run on NULL, and the decoders themselves compare
-- types, so NULL of any type decodes to 'Nothing'. This includes an untyped
-- @NULL@ literal, which is of the text type as far as the server is concerned.
--
-- On NULL the combinator consumes exactly one field, so the given decoder must
-- consume exactly one field as well. All decoders defined in this module do.
-- A decoder that consumes a different number of fields results in
-- t'RowLengthMismatch'.
decodeNullable :: RowDecoder a -> RowDecoder (Maybe a)
decodeNullable (RowDecoder inner) = mkDecoder $ do
  DecoderState fs idx <- get
  boundsCheck fs idx
  isnull <- lift $ fs.isNull idx
  if isnull
    then do
      put $! DecoderState fs (idx + 1)
      pure Nothing
    else do
      (a, st@(DecoderState _ consumed)) <- lift . inner $ DecoderState fs idx
      -- The NULL branch above consumes exactly one field. If the given decoder
      -- consumed a different number, decoding would advance by a
      -- NULL-dependent number of fields.
      when (consumed /= idx + 1) . lift . E.throwIO $
        RowLengthMismatch
          { lengthExpected = consumed - idx
          , lengthDelivered = 1
          }
      put st
      pure $ Just a

-- | Decode the next field with a value decoder from
-- "Database.PostgreSQL.PQTypes.Internal.Decoding". If the type of the field
-- differs from the expected one, throw t'TypeMismatch'.
decodeScalar :: forall a. PQFormat a => D.Value a -> RowDecoder a
decodeScalar valueDec = withNextField $ \_srcRow -> \case
  Field oid mvalue -> do
    when (oid /= pqOid @a) . E.throwIO $
      TypeMismatch
        { tmExpectedOid = pqOid @a
        , tmDeliveredOid = oid
        }
    case mvalue of
      Nothing -> unexpectedNULL
      Just value -> getParseResult "decodeScalar" $ D.valueParser valueDec value
  SubArray ad -> E.throwIO $ subArrayDimensionMismatch ad
-- Inlined so that 'FromSQL' instances specialize the decoder to the decoded
-- type. In particular they resolve the type class dictionaries of polymorphic
-- parsers such as 'D.int'. This speeds decoding up by 5-10%.
{-# INLINE decodeScalar #-}

-- | Decode the next field as a value of a PostgreSQL enum type with the given
-- map from enum labels to values. The field can also be of the text type,
-- because the binary wire format of enums and text is the same.
decodeEnum :: Map T.Text a -> RowDecoder a
decodeEnum values = withNextField $ \_srcRow -> \case
  Field oid mvalue -> do
    -- Enum types are user-defined, so their OIDs aren't known statically.
    -- They are always at least 'firstNormalOid' though, so this rejects at
    -- least built-in non-text types. The lookup in the map below rejects
    -- wrong labels anyway.
    when (oid /= textOid && oid < firstNormalOid) . E.throwIO $
      TypeMismatch
        { tmExpectedOid = textOid
        , tmDeliveredOid = oid
        }
    case mvalue of
      Nothing -> unexpectedNULL
      Just value -> do
        label <- getParseResult "decodeEnum" $ D.valueParser D.text_strict value
        case Map.lookup label values of
          Just a -> pure a
          Nothing ->
            E.throwIO
              InvalidValue
                { ivValue = label
                , ivValidValues = Just $ Map.keys values
                }
  SubArray ad -> E.throwIO $ subArrayDimensionMismatch ad

-- | Decode the next field as a composite value, i.e. an anonymous row or a
-- user-defined composite type, with the given decoder of its fields. The
-- decoder must consume all fields of the composite:
--
-- > -- select 1::int, (2::int, 'hi'::text), true::bool
-- > decoder :: RowDecoder (Int32, (Int32, T.Text), Bool)
-- > decoder = (,,) <$> fromSQL <*> decodeComposite ((,) <$> fromSQL <*> fromSQL) <*> fromSQL
decodeComposite :: RowDecoder a -> RowDecoder a
decodeComposite inner = withNextField $ \srcRow -> \case
  Field oid mvalue -> do
    -- The field must be an anonymous record or a user-defined composite type.
    -- OIDs of the latter aren't known statically. They are always at least
    -- 'firstNormalOid' though, so this rejects at least built-in non-record
    -- types. The inner decoder compares the types of the fields of the record.
    when (oid /= recordOid && oid < firstNormalOid) . E.throwIO $
      TypeMismatch
        { tmExpectedOid = recordOid
        , tmDeliveredOid = oid
        }
    case mvalue of
      Nothing -> unexpectedNULL
      Just value -> do
        fields <- getParseResult "decodeComposite" $ parseRecord value
        runDecoderWith inner $ mkFieldSource srcRow (V.length fields) (fields V.!)
  SubArray ad -> E.throwIO $ subArrayDimensionMismatch ad

-- | Decode the next field as an array with the given decoder of its elements.
-- Nesting decodes a multi-dimensional array, e.g.
-- @'decodeArray' ('decodeArray' fromSQL)@ decodes a two-dimensional array as
-- a vector of vectors.
--
-- An error of an element decoder carries the position of the offending
-- element as the column of its t'ConversionError'.
decodeArray :: forall a. RowDecoder a -> RowDecoder (V.Vector a)
decodeArray (RowDecoder inner) = withNextField $ \srcRow -> \case
  Field _ Nothing -> unexpectedNULL
  Field _ (Just value) -> do
    -- Like 'decodeComposite', this doesn't compare the type of the field
    -- itself, because the array OID of the element type isn't known here.
    -- The inner decoder compares the types of the elements.
    arrayData <- getParseResult "decodeArray" $ parseArray value
    decodeArrayData srcRow arrayData
  SubArray arrayData -> decodeArrayData srcRow arrayData
  where
    -- The wire format is flat: a header with the list of dimensions, then all
    -- the elements in row-major order. Each application of 'decodeArray'
    -- peels one dimension off dims. A 'SubArray' carries a slice of the flat
    -- element vector that holds one complete sub-array to the nested
    -- 'decodeArray'. This is why the 'SubArray' branch above parses nothing.
    decodeArrayData :: Int -> ArrayData -> IO (V.Vector a)
    decodeArrayData srcRow ad = case ad.dims of
      [] -> pure V.empty
      [n] -> decodeElems srcRow n $ \i ->
        Field ad.elemOid $ ad.elems V.! i
      n : rest -> do
        let chunkLen = product rest
        decodeElems srcRow n $ \i ->
          SubArray . ArrayData ad.elemOid rest $ V.slice (i * chunkLen) chunkLen ad.elems

    -- All elements share a single field source, with mkField (the lambdas at
    -- the call sites above) as its getField. This way the source constructs
    -- the Field value that wraps the payload of an element only when the
    -- element decoder asks for it, and the value becomes garbage right after.
    -- A vector of Field values for all elements allocated upfront (e.g. with
    -- V.map) proved to be a pessimization.
    decodeElems :: Int -> Int -> (Int -> Field) -> IO (V.Vector a)
    decodeElems srcRow n mkField = do
      let fs = mkFieldSource srcRow n mkField
      V.generateM n $ \i -> do
        -- The element decoder starts at the index of its element. It must end
        -- exactly one field later.
        (a, DecoderState _ consumed) <- inner $ DecoderState fs i
        when (consumed /= i + 1) . E.throwIO $
          RowLengthMismatch
            { lengthExpected = consumed - i
            , lengthDelivered = 1
            }
        pure a

-- | Decode the next field as a one-dimensional array of a scalar type with
-- the given value decoder of its elements.
--
-- Equivalent to @'decodeArray' ('decodeScalar' valueDec)@, but faster: the
-- value decoder decodes the elements directly, without the generic machinery
-- of 'decodeArray'. The type of the elements is compared once for the whole
-- array instead of per element, which is equivalent because all elements
-- share it.
--
-- Errors of the elements report their position the same way 'decodeArray'
-- does, i.e. as the column of a t'ConversionError'.
decodeScalarArray :: forall a. PQFormat a => D.Value a -> RowDecoder (V.Vector a)
decodeScalarArray valueDec = withNextField $ \srcRow -> \case
  Field _ Nothing -> unexpectedNULL
  Field _ (Just value) -> do
    ad <- getParseResult "decodeScalarArray" $ parseArray value
    decodeArrayData srcRow ad
  SubArray ad -> decodeArrayData srcRow ad
  where
    decodeArrayData srcRow ad = case ad.dims of
      [] -> pure V.empty
      [n] -> do
        when (ad.elemOid /= pqOid @a) . E.throwIO $
          TypeMismatch
            { tmExpectedOid = pqOid @a
            , tmDeliveredOid = ad.elemOid
            }
        V.generateM n $ \i -> case ad.elems V.! i of
          Nothing -> atElement srcRow i unexpectedNULL
          Just value -> case D.valueParser valueDec value of
            Right a -> pure a
            Left err ->
              atElement srcRow i . getParseResult "decodeScalarArray" $ Left err
      -- In 'decodeArray', the element decoder gets the remaining dimensions
      -- as a sub-array, which a scalar decoder rejects. This reports the same
      -- error, because the two are meant to be interchangeable.
      _ : rest ->
        E.throwIO
          ArrayDimensionMismatch
            { arrDimExpected = 0
            , arrDimDelivered = length rest
            }

    -- Attach the position of an element to the error of its decoder, like
    -- 'withNextField' does for the elements of 'decodeArray'. Only the
    -- branches that already fail install the handler, so the elements that
    -- succeed don't pay for it.
    atElement :: Int -> Int -> IO b -> IO b
    atElement srcRow i action =
      action `catchSync` \(E.SomeException err) ->
        E.throwIO
          ConversionError
            { convColumn = i + 1
            , convColumnName = ""
            , convRow = srcRow + 1
            , convError = err
            }
-- See the comment at 'decodeScalar'.
{-# INLINE decodeScalarArray #-}

----------------------------------------
-- Helpers

-- | Consume the next field and process it. This wraps a synchronous exception
-- thrown in the process in t'ConversionError' with the position of the field.
withNextField :: (Int -> Field -> IO a) -> RowDecoder a
withNextField process = mkDecoder $ do
  DecoderState fs idx <- get
  boundsCheck fs idx
  put $! DecoderState fs (idx + 1)
  lift $
    (process fs.row =<< fs.getField idx) `catchSync` \(E.SomeException err) -> do
      name <- fs.fieldName idx
      E.throwIO
        ConversionError
          { convColumn = idx + 1
          , convColumnName = name
          , convRow = fs.row + 1
          , convError = err
          }

boundsCheck :: FieldSource -> Int -> StateT DecoderState IO ()
boundsCheck fs idx =
  when (idx >= fs.numFields) . lift . E.throwIO $
    RowLengthMismatch
      { lengthExpected = idx + 1
      , lengthDelivered = fs.numFields
      }

-- | Get a result of running a parser or throw an exception.
getParseResult :: String -> Either T.Text a -> IO a
getParseResult fun = either (hpqTypesError . (fun ++) . (": " ++) . T.unpack) pure

subArrayDimensionMismatch :: ArrayData -> ArrayDimensionMismatch
subArrayDimensionMismatch ad =
  ArrayDimensionMismatch
    { arrDimExpected = 0
    , arrDimDelivered = length ad.dims
    }

----------------------------------------

-- Parsers of the binary wire format of records and arrays. Reference:
-- record_send and array_send in the PostgreSQL sources.

-- | Parse a record value: the number of fields (32-bit integer), then the
-- fields. Each field is its type (32-bit OID), then its length-prefixed value
-- (see 'getValue').
parseRecord :: BS.ByteString -> Either T.Text (V.Vector Field)
parseRecord = BP.run $ do
  numFields <- BP.beWord32
  fields <- V.replicateM (fromIntegral numFields) $ do
    oid <- BP.beWord32
    Field (Oid oid) <$> getValue
  fields <$ BP.endOfInput

-- | Parse an array value. The wire format is the number of dimensions (32-bit
-- integer), the presence of NULLs (32-bit integer, ignored) and the type of
-- the elements (32-bit OID). Then for each dimension its size (32-bit integer)
-- and lower bound (32-bit integer, ignored). Then the elements in row-major
-- order, each as a length-prefixed value (see 'getValue').
--
-- Garbage dimensions from data that is not an array can't cause unbounded
-- allocation. The number of elements is capped at the maximum the server can
-- send. The parser rejects a dimension of size 0: it makes the product 0, so
-- the sizes of the other dimensions escape that cap. A read past the end of
-- input fails and aborts the traversal.
parseArray :: BS.ByteString -> Either T.Text ArrayData
parseArray = BP.run $ do
  numDims <- BP.beWord32
  -- Skip the has-nulls flag.
  BP.unitOfSize 4
  elemOid <- BP.beWord32
  -- The upper bound is MAXDIM from the PostgreSQL sources.
  when (numDims > 6) $ BP.failure "invalid number of dimensions"
  dims <- replicateM (fromIntegral numDims) $ do
    dim <- BP.beWord32
    -- Skip the lower bound.
    BP.unitOfSize 4
    -- Empty arrays have zero dimensions, so a dimension of size 0 never
    -- appears on the wire.
    when (dim == 0) $ BP.failure "invalid dimension size"
    pure $ fromIntegral dim
  -- The product is an Integer to rule out overflow.
  let numElems = if null dims then 0 else product (map toInteger dims)
  when (numElems > maxArraySize) $ BP.failure "invalid number of elements"
  elems <- V.replicateM (fromIntegral numElems) getValue
  BP.endOfInput
  pure
    ArrayData
      { elemOid = Oid elemOid
      , dims = dims
      , elems = elems
      }
  where
    -- Maximum number of elements in an array: MaxArraySize from the
    -- PostgreSQL sources, defined there as the maximum allocation size
    -- divided by the size of a Datum.
    maxArraySize :: Integer
    maxArraySize = 0x3fffffff `div` 8

-- | Read a length-prefixed value: a 32-bit length, then that many bytes. The
-- maximum unsigned value, i.e. -1 when read as signed, represents NULL.
getValue :: BP.BinaryParser (Maybe BS.ByteString)
getValue = do
  len <- BP.beWord32
  if len == maxBound -- -1, i.e. NULL
    then pure Nothing
    else Just <$> BP.bytesOfSize (fromIntegral len)
