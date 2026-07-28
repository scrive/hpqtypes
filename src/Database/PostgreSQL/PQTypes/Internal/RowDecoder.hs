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
import PostgreSQL.Binary.Decoding qualified as D

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Oid
import Database.PostgreSQL.PQTypes.Internal.Utils

type role RowDecoder representational

-- | Decoder of values of type @a@ from fields of a query result. It's a
-- monad, so a decoder of a compound value is built from decoders of its
-- fields sequentially:
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

-- | Exceptions thrown with 'throwM' are wrapped in 'ConversionError' carrying
-- the position of the most recently consumed field, like the errors thrown by
-- the decoders of the fields themselves. This makes it the way to report errors
-- detected after a field was decoded, e.g. by validation of the decoded value.
--
-- /Note:/ an error thrown before the field it concerns was consumed is
-- attributed to the preceding field (or carries no position at all when thrown
-- before any field was consumed).
instance MonadThrow RowDecoder where
  throwM err = mkDecoder $ do
    DecoderState fs idx <- get
    lift $
      if idx == 0
        -- No field was consumed yet, so there's no position to attach.
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

-- | Internal helper for defining decoders in terms of 'StateT'.
mkDecoder :: StateT DecoderState IO a -> RowDecoder a
mkDecoder = RowDecoder . runStateT

-- | State of the decoder: the source of fields along with the index of the
-- next field to decode. Note that only the index changes as decoding
-- progresses.
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

-- | A field source backed by a function that produces fields. Note that
-- the strictness annotations of 'FieldSource' force only the function
-- closures, so a 'Field' is only constructed when a decoder asks for it.
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

-- | Run a decoder against a row of a query result, checking that it consumed
-- all its columns.
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

-- | Run a decoder against a field source, checking that it consumed all the
-- fields.
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

-- | Decode the next field with the given decoder, mapping NULL to 'Nothing'.
--
-- The given decoder doesn't run on NULL, so the type of the field is then
-- not checked (it's the decoders themselves that check types): NULL of any
-- type, including an untyped @NULL@ literal (which is of the text type as
-- far as the server is concerned), decodes to 'Nothing'.
--
-- Note that on NULL exactly one field is consumed, hence the given decoder
-- needs to consume exactly one field as well, which is the case for all
-- decoders defined in this module. This is enforced: a decoder consuming a
-- different number of fields results in 'RowLengthMismatch'.
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
      -- The NULL branch above consumes exactly one field, so it needs to be
      -- checked that the given decoder did the same, otherwise decoding
      -- would advance by a NULL-dependent number of fields.
      when (consumed /= idx + 1) . lift . E.throwIO $
        RowLengthMismatch
          { lengthExpected = consumed - idx
          , lengthDelivered = 1
          }
      put st
      pure $ Just a

-- | Decode the next field using a value decoder from
-- "PostgreSQL.Binary.Decoding", after verifying that the type of the field
-- matches the expected one.
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
-- type (in particular resolve the type class dictionaries of polymorphic
-- parsers such as 'D.int'), which improves the decoding speed by 5-10%.
{-# INLINE decodeScalar #-}

-- | Decode the next field as a value of a PostgreSQL enum type, using the
-- given map from enum labels to values. The field can also be of the text
-- type, as the binary wire format of enums and text is the same.
decodeEnum :: Map T.Text a -> RowDecoder a
decodeEnum values = withNextField $ \_srcRow -> \case
  Field oid mvalue -> do
    -- Enum types are user-defined, so their OIDs cannot be known statically,
    -- but they are always greater than or equal to 'firstNormalOid', so at
    -- least decoding of built-in non-text types is rejected here. Labels are
    -- verified against the map below anyway.
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

-- | Decode the next field as a composite (anonymous row or user-defined)
-- value, using the supplied decoder for decoding its fields. The decoder
-- needs to use all fields of the composite:
--
-- > -- select 1::int, (2::int, 'hi'::text), true::bool
-- > decoder :: RowDecoder (Int32, (Int32, T.Text), Bool)
-- > decoder = (,,) <$> fromSQL <*> decodeComposite ((,) <$> fromSQL <*> fromSQL) <*> fromSQL
decodeComposite :: RowDecoder a -> RowDecoder a
decodeComposite inner = withNextField $ \srcRow -> \case
  Field oid mvalue -> do
    -- The field needs to be either an anonymous record or a user-defined
    -- composite type. OIDs of the latter cannot be known statically, but
    -- they are always greater than or equal to 'firstNormalOid', so at least
    -- decoding of built-in non-record types is rejected here. Types of the
    -- fields of the record are verified by the inner decoder.
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

-- | Decode the next field as an array, using the supplied decoder for
-- decoding its elements. Multi-dimensional arrays are decoded by nesting,
-- e.g. @'decodeArray' ('decodeArray' fromSQL)@ decodes a two-dimensional
-- array as a vector of vectors.
--
-- Errors from element decoders carry the position of the offending element
-- as the column of their 'ConversionError'.
decodeArray :: forall a. RowDecoder a -> RowDecoder (V.Vector a)
decodeArray (RowDecoder inner) = withNextField $ \srcRow -> \case
  Field _ Nothing -> unexpectedNULL
  Field _ (Just value) -> do
    -- Like with 'decodeComposite', the type of the field itself is not
    -- checked (the array OID corresponding to the type of the elements is
    -- not known here), but types of the elements are verified by the inner
    -- decoder.
    arrayData <- getParseResult "decodeArray" $ parseArray value
    decodeArrayData srcRow arrayData
  SubArray arrayData -> decodeArrayData srcRow arrayData
  where
    -- The wire format is flat: a header with the list of dimensions,
    -- followed by all the elements in row-major order. Each application of
    -- 'decodeArray' peels one dimension off dims, and 'SubArray' is how a
    -- slice of the flat element vector holding one complete sub-array
    -- travels to the nested 'decodeArray' (which is why the 'SubArray'
    -- branch above parses nothing).
    decodeArrayData :: Int -> ArrayData -> IO (V.Vector a)
    decodeArrayData srcRow ad = case ad.dims of
      [] -> pure V.empty
      [n] -> decodeElems srcRow n $ \i ->
        Field ad.elemOid $ ad.elems V.! i
      n : rest -> do
        let chunkLen = product rest
        decodeElems srcRow n $ \i ->
          SubArray . ArrayData ad.elemOid rest $ V.slice (i * chunkLen) chunkLen ad.elems

    -- All elements are decoded against a single field source, with mkField
    -- (the lambdas at the call sites above) as its getField. This way the
    -- Field value wrapping the payload of an element is only constructed
    -- when the element decoder asks for it and becomes garbage right after:
    -- allocating a vector of Field values for all elements upfront (e.g.
    -- with V.map) proved to be a pessimization.
    decodeElems :: Int -> Int -> (Int -> Field) -> IO (V.Vector a)
    decodeElems srcRow n mkField = do
      let fs = mkFieldSource srcRow n mkField
      V.generateM n $ \i -> do
        -- The element decoder starts at the index of its element, so it
        -- needs to be checked that it consumed exactly one field.
        (a, DecoderState _ consumed) <- inner $ DecoderState fs i
        when (consumed /= i + 1) . E.throwIO $
          RowLengthMismatch
            { lengthExpected = consumed - i
            , lengthDelivered = 1
            }
        pure a

----------------------------------------
-- Helpers

-- | Consume the next field and process it. Any synchronous exception thrown
-- while doing so is wrapped in 'ConversionError' with the position of the
-- field attached.
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

-- | Parse a record value: the number of fields (32-bit integer) followed by
-- the fields, each with its type (32-bit OID) followed by its
-- length-prefixed value (see 'getValue').
parseRecord :: BS.ByteString -> Either T.Text (V.Vector Field)
parseRecord = BP.run $ do
  numFields <- BP.beWord32
  fields <- V.replicateM (fromIntegral numFields) $ do
    oid <- BP.beWord32
    Field (Oid oid) <$> getValue
  fields <$ BP.endOfInput

-- | Parse an array value: the number of dimensions (32-bit integer), the
-- presence of NULLs (32-bit integer, ignored), the type of the elements
-- (32-bit OID), then for each dimension its size (32-bit integer) and lower
-- bound (32-bit integer, ignored), then the elements in row-major order,
-- each as a length-prefixed value (see 'getValue').
--
-- Note that garbage dimensions resulting from parsing data that is not an
-- array cannot cause unbounded allocation: the number of elements is capped
-- at the maximum the server can send, dimensions of size 0 (which would
-- exempt the sizes of the other dimensions from that cap) are rejected and
-- reading past the end of input fails, which aborts the traversal.
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
  -- The number of elements is computed as an Integer to rule out overflow.
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

-- | Read a length-prefixed value: a 32-bit length (with the maximum unsigned
-- value, i.e. -1 when read as signed, representing NULL) followed by that
-- many bytes.
getValue :: BP.BinaryParser (Maybe BS.ByteString)
getValue = do
  len <- BP.beWord32
  if len == maxBound -- -1, i.e. NULL
    then pure Nothing
    else Just <$> BP.bytesOfSize (fromIntegral len)
