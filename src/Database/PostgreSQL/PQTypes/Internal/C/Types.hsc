-- | Mappings of types used by libpq/libpqtypes to Haskell ADTs.
module Database.PostgreSQL.PQTypes.Internal.C.Types (
    PGcancel
  , PGconn
  , PGparam
  , PGresult
  , PGtypeArgs
  , ConnStatusType(..)
  , c_CONNECTION_OK, c_CONNECTION_BAD, c_CONNECTION_STARTED
  , c_CONNECTION_MADE, c_CONNECTION_AWAITING_RESPONSE, c_CONNECTION_AUTH_OK
  , c_CONNECTION_SETENV, c_CONNECTION_SSL_STARTUP, c_CONNECTION_NEEDED
  , PostgresPollingStatusType(..)
  , c_PGRES_POLLING_FAILED, c_PGRES_POLLING_READING, c_PGRES_POLLING_WRITING
  , c_PGRES_POLLING_OK, c_PGRES_POLLING_ACTIVE
  , ResultFormat(..)
  , c_RESULT_TEXT, c_RESULT_BINARY
  , ExecStatusType(..)
  , c_PGRES_EMPTY_QUERY, c_PGRES_COMMAND_OK, c_PGRES_TUPLES_OK
  , c_PGRES_COPY_OUT, c_PGRES_COPY_IN, c_PGRES_BAD_RESPONSE
  , c_PGRES_NONFATAL_ERROR, c_PGRES_FATAL_ERROR, c_PGRES_COPY_BOTH
  , ErrorField(..)
  , c_PG_DIAG_SEVERITY, c_PG_DIAG_SQLSTATE, c_PG_DIAG_MESSAGE_PRIMARY
  , c_PG_DIAG_MESSAGE_DETAIL, c_PG_DIAG_MESSAGE_HINT
  , c_PG_DIAG_STATEMENT_POSITION, c_PG_DIAG_INTERNAL_POSITION
  , c_PG_DIAG_INTERNAL_QUERY, c_PG_DIAG_CONTEXT, c_PG_DIAG_SOURCE_FILE
  , c_PG_DIAG_SOURCE_LINE, c_PG_DIAG_SOURCE_FUNCTION
  , TypeClass(..)
  , c_PQT_SUBCLASS, c_PQT_COMPOSITE, c_PQT_USERDEFINED
  , PGerror(..)
  , PGregisterType(..)
  , PGarray(..)
  , PGbytea(..)
  , PGuuid(..)
  , PGdate(..)
  , PGtime(..)
  , PGtimestamp(..)
  ) where

import Data.Word
import Data.ByteString.Unsafe
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V

data {-# CTYPE "libpqtypes.h" "PGcancel" #-} PGcancel
data {-# CTYPE "libpqtypes.h" "PGconn" #-} PGconn
data {-# CTYPE "libpqtypes.h" "PGparam" #-} PGparam
data {-# CTYPE "libpqtypes.h" "PGresult" #-} PGresult
data {-# CTYPE "libpqtypes.h" "PGtypeArgs" #-} PGtypeArgs

#include <libpqtypes.h>
#include <libpq-fe.h>

foreign import capi unsafe "libpqtypes.h htonl" htonl :: Word32 -> Word32
foreign import capi unsafe "libpqtypes.h ntohl" ntohl :: Word32 -> Word32

----------------------------------------

newtype ConnStatusType = ConnStatusType CInt
  deriving Eq

#{enum ConnStatusType, ConnStatusType
, c_CONNECTION_OK = CONNECTION_OK
, c_CONNECTION_BAD = CONNECTION_BAD
, c_CONNECTION_STARTED = CONNECTION_STARTED
, c_CONNECTION_MADE = CONNECTION_MADE
, c_CONNECTION_AWAITING_RESPONSE = CONNECTION_AWAITING_RESPONSE
, c_CONNECTION_AUTH_OK = CONNECTION_AUTH_OK
, c_CONNECTION_SETENV = CONNECTION_SETENV
, c_CONNECTION_SSL_STARTUP = CONNECTION_SSL_STARTUP
, c_CONNECTION_NEEDED = CONNECTION_NEEDED
}

----------------------------------------

newtype PostgresPollingStatusType = PostgresPollingStatusType CInt
  deriving Eq

#{enum PostgresPollingStatusType, PostgresPollingStatusType
 , c_PGRES_POLLING_FAILED  = PGRES_POLLING_FAILED
 , c_PGRES_POLLING_READING = PGRES_POLLING_READING
 , c_PGRES_POLLING_WRITING = PGRES_POLLING_WRITING
 , c_PGRES_POLLING_OK      = PGRES_POLLING_OK
 , c_PGRES_POLLING_ACTIVE  = PGRES_POLLING_ACTIVE
 }

----------------------------------------

newtype ResultFormat = ResultFormat CInt

c_RESULT_TEXT :: ResultFormat
c_RESULT_TEXT = ResultFormat 0

c_RESULT_BINARY :: ResultFormat
c_RESULT_BINARY = ResultFormat 1

----------------------------------------

newtype ExecStatusType = ExecStatusType CInt
  deriving Eq

#{enum ExecStatusType, ExecStatusType
, c_PGRES_EMPTY_QUERY = PGRES_EMPTY_QUERY
, c_PGRES_COMMAND_OK = PGRES_COMMAND_OK
, c_PGRES_TUPLES_OK = PGRES_TUPLES_OK
, c_PGRES_COPY_OUT = PGRES_COPY_OUT
, c_PGRES_COPY_IN = PGRES_COPY_IN
, c_PGRES_BAD_RESPONSE = PGRES_BAD_RESPONSE
, c_PGRES_NONFATAL_ERROR = PGRES_NONFATAL_ERROR
, c_PGRES_FATAL_ERROR = PGRES_FATAL_ERROR
, c_PGRES_COPY_BOTH = PGRES_COPY_BOTH
}

----------------------------------------

newtype ErrorField = ErrorField CInt
  deriving Eq

#{enum ErrorField, ErrorField
, c_PG_DIAG_SEVERITY = PG_DIAG_SEVERITY
, c_PG_DIAG_SQLSTATE = PG_DIAG_SQLSTATE
, c_PG_DIAG_MESSAGE_PRIMARY = PG_DIAG_MESSAGE_PRIMARY
, c_PG_DIAG_MESSAGE_DETAIL = PG_DIAG_MESSAGE_DETAIL
, c_PG_DIAG_MESSAGE_HINT = PG_DIAG_MESSAGE_HINT
, c_PG_DIAG_STATEMENT_POSITION = PG_DIAG_STATEMENT_POSITION
, c_PG_DIAG_INTERNAL_POSITION = PG_DIAG_INTERNAL_POSITION
, c_PG_DIAG_INTERNAL_QUERY = PG_DIAG_INTERNAL_QUERY
, c_PG_DIAG_CONTEXT = PG_DIAG_CONTEXT
, c_PG_DIAG_SOURCE_FILE = PG_DIAG_SOURCE_FILE
, c_PG_DIAG_SOURCE_LINE = PG_DIAG_SOURCE_LINE
, c_PG_DIAG_SOURCE_FUNCTION = PG_DIAG_SOURCE_FUNCTION
}

----------------------------------------

newtype TypeClass = TypeClass CInt
  deriving Eq

#{enum TypeClass, TypeClass
, c_PQT_SUBCLASS = PQT_SUBCLASS
, c_PQT_COMPOSITE = PQT_COMPOSITE
, c_PQT_USERDEFINED = PQT_USERDEFINED
}

----------------------------------------

newtype PGerror = PGerror {
  pgErrorMsg :: String
} deriving Show

instance Storable PGerror where
  sizeOf _ = #{size PGerror}
  alignment _ = #{alignment PGerror}
  peek ptr = PGerror <$> peekCString (#{ptr PGerror, msg} ptr)
  poke _ _ = error "Storable PGerror: poke is not supposed to be used"

----------------------------------------

data PGregisterType = PGregisterType
  { pgRegisterTypeTypName :: !CString
  , pgRegisterTypeTypPut  :: !(FunPtr (Ptr PGtypeArgs -> IO CInt))
  , pgRegisterTypeTypGet  :: !(FunPtr (Ptr PGtypeArgs -> IO CInt))
  } deriving Show

instance Storable PGregisterType where
  sizeOf _ = #{size PGregisterType}
  alignment _ = #{alignment PGregisterType}
  peek ptr = PGregisterType
    <$> #{peek PGregisterType, typname} ptr
    <*> #{peek PGregisterType, typput} ptr
    <*> #{peek PGregisterType, typget} ptr
  poke ptr PGregisterType{..} = do
    #{poke PGregisterType, typname} ptr pgRegisterTypeTypName
    #{poke PGregisterType, typput} ptr pgRegisterTypeTypPut
    #{poke PGregisterType, typget} ptr pgRegisterTypeTypGet

----------------------------------------

c_MAXDIM :: Int
c_MAXDIM = #{const MAXDIM}

data PGarray = PGarray
  { pgArrayNDims  :: !CInt
  , pgArrayLBound :: !(V.Vector CInt)
  , pgArrayDims   :: !(V.Vector CInt)
  , pgArrayParam  :: !(Ptr PGparam)
  , pgArrayRes    :: !(Ptr PGresult)
  } deriving Show

instance Storable PGarray where
  sizeOf _ = #{size PGarray}
  alignment _ = #{alignment PGarray}
  peek ptr = PGarray
    <$> #{peek PGarray, ndims} ptr
    <*> readVector (#{ptr PGarray, lbound} ptr)
    <*> readVector (#{ptr PGarray, dims} ptr)
    <*> #{peek PGarray, param} ptr
    <*> #{peek PGarray, res} ptr
    where
      readVector :: Ptr CInt -> IO (V.Vector CInt)
      readVector src = do
        let len = c_MAXDIM
        fptr <- mallocForeignPtrArray len
        withForeignPtr fptr $ \dest -> copyArray dest src len
        return (V.unsafeFromForeignPtr0 fptr len)

  poke ptr PGarray{..} = do
    #{poke PGarray, ndims} ptr pgArrayNDims
    writeVector pgArrayLBound $ #{ptr PGarray, lbound} ptr
    writeVector pgArrayDims $ #{ptr PGarray, dims} ptr
    #{poke PGarray, param} ptr pgArrayParam
    #{poke PGarray, res} ptr pgArrayRes
    where
      writeVector :: V.Vector CInt -> Ptr CInt -> IO ()
      writeVector v dest = do
        let (fptr, baseLen) = V.unsafeToForeignPtr0 v
        withForeignPtr fptr $ \src -> do
          let len = min baseLen c_MAXDIM
          copyArray dest src len

----------------------------------------

data PGbytea = PGbytea
  { pgByteaLen  :: !CInt
  , pgByteaData :: !CString
  } deriving Show

instance Storable PGbytea where
  sizeOf _ = #{size PGbytea}
  alignment _ = #{alignment PGbytea}
  peek ptr = PGbytea
    <$> #{peek PGbytea, len} ptr
    <*> #{peek PGbytea, data} ptr
  poke ptr PGbytea{..} = do
    #{poke PGbytea, len} ptr pgByteaLen
    #{poke PGbytea, data} ptr pgByteaData

----------------------------------------

-- Same as the UUID type from uuid-types package except for the Storable
-- instance: PostgreSQL expects the binary representation to be encoded in
-- network byte order (as per RFC 4122), whereas Storable instance of UUID
-- preserves host byte order, so we need to have our own version.
data PGuuid = PGuuid
  { pgUuidW1 :: !Word32
  , pgUuidW2 :: !Word32
  , pgUuidW3 :: !Word32
  , pgUuidW4 :: !Word32
  } deriving Show

instance Storable PGuuid where
  sizeOf _ = #{size PGuuid}
  alignment _ = #{alignment PGuuid}

  peek ptr = PGuuid
    <$> (ntohl <$> #{peek PGuuid, w1} ptr)
    <*> (ntohl <$> #{peek PGuuid, w2} ptr)
    <*> (ntohl <$> #{peek PGuuid, w3} ptr)
    <*> (ntohl <$> #{peek PGuuid, w4} ptr)

  poke ptr PGuuid{..} = do
    #{poke PGuuid, w1} ptr $ htonl pgUuidW1
    #{poke PGuuid, w2} ptr $ htonl pgUuidW2
    #{poke PGuuid, w3} ptr $ htonl pgUuidW3
    #{poke PGuuid, w4} ptr $ htonl pgUuidW4

data PGdate = PGdate
  { pgDateIsBC :: !CInt
  , pgDateYear :: !CInt
  , pgDateMon  :: !CInt
  , pgDateMDay :: !CInt
  , pgDateJDay :: !CInt
  , pgDateYDay :: !CInt
  , pgDateWDay :: !CInt
  } deriving Show

instance Storable PGdate where
  sizeOf _ = #{size PGdate}
  alignment _ = #{alignment PGdate}
  peek ptr = PGdate
    <$> #{peek PGdate, isbc} ptr
    <*> #{peek PGdate, year} ptr
    <*> #{peek PGdate, mon}  ptr
    <*> #{peek PGdate, mday} ptr
    <*> #{peek PGdate, jday} ptr
    <*> #{peek PGdate, yday} ptr
    <*> #{peek PGdate, wday} ptr
  poke ptr PGdate{..} = do
    #{poke PGdate, isbc} ptr pgDateIsBC
    #{poke PGdate, year} ptr pgDateYear
    #{poke PGdate, mon}  ptr pgDateMon
    #{poke PGdate, mday} ptr pgDateMDay
    #{poke PGdate, jday} ptr pgDateJDay
    #{poke PGdate, yday} ptr pgDateYDay
    #{poke PGdate, wday} ptr pgDateWDay

----------------------------------------

data PGtime = PGtime
  { pgTimeHour   :: !CInt
  , pgTimeMin    :: !CInt
  , pgTimeSec    :: !CInt
  , pgTimeUSec   :: !CInt
  , pgTimeWithTZ :: !CInt
  , pgTimeIsDST  :: !CInt
  , pgTimeGMTOff :: !CInt
  , pgTimeTZAbbr :: !BS.ByteString
  } deriving Show

instance Storable PGtime where
  sizeOf _ = #{size PGtime}
  alignment _ = #{alignment PGtime}
  peek ptr = PGtime
    <$> #{peek PGtime, hour}   ptr
    <*> #{peek PGtime, min}    ptr
    <*> #{peek PGtime, sec}    ptr
    <*> #{peek PGtime, usec}   ptr
    <*> #{peek PGtime, withtz} ptr
    <*> #{peek PGtime, isdst}  ptr
    <*> #{peek PGtime, gmtoff} ptr
    <*> BS.packCString (#{ptr PGtime, tzabbr} ptr)
  poke ptr PGtime{..} = do
    #{poke PGtime, hour}   ptr pgTimeHour
    #{poke PGtime, min}    ptr pgTimeMin
    #{poke PGtime, sec}    ptr pgTimeSec
    #{poke PGtime, usec}   ptr pgTimeUSec
    #{poke PGtime, withtz} ptr pgTimeWithTZ
    #{poke PGtime, isdst}  ptr pgTimeIsDST
    #{poke PGtime, gmtoff} ptr pgTimeGMTOff
    unsafeUseAsCStringLen pgTimeTZAbbr $ \(cs, len) -> do
      let tzabbr = #{ptr PGtime, tzabbr} ptr
      copyArray tzabbr cs (min len 16)
      pokeElemOff tzabbr (min len 15) (0::CChar)

----------------------------------------

data PGtimestamp = PGtimestamp
  { pgTimestampEpoch :: !CLLong
  , pgTimestampDate  :: !PGdate
  , pgTimestampTime  :: !PGtime
  } deriving Show

instance Storable PGtimestamp where
  sizeOf _ = #{size PGtimestamp}
  alignment _ = #{alignment PGtimestamp}
  peek ptr = PGtimestamp
    <$> #{peek PGtimestamp, epoch} ptr
    <*> #{peek PGtimestamp, date}  ptr
    <*> #{peek PGtimestamp, time}  ptr
  poke ptr PGtimestamp{..} = do
    #{poke PGtimestamp, epoch} ptr pgTimestampEpoch
    #{poke PGtimestamp, date}  ptr pgTimestampDate
    #{poke PGtimestamp, time}  ptr pgTimestampTime
