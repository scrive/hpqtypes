{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, RecordWildCards #-}
module DB.Primitive.Types where

import Control.Applicative
import Data.ByteString.Unsafe
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString as BS

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__);}, y__)

data PGconn
data PGparam
data PGresult
data PGparam

newtype QueryResult = QueryResult { unQueryResult :: ForeignPtr PGresult }

#include <libpqtypes.h>

data PGdate = PGdate {
  pgDateIsBC :: {-# UNPACK #-} !CInt
, pgDateYear :: {-# UNPACK #-} !CInt
, pgDateMon  :: {-# UNPACK #-} !CInt
, pgDateMDay :: {-# UNPACK #-} !CInt
, pgDateJDay :: {-# UNPACK #-} !CInt
, pgDateYDay :: {-# UNPACK #-} !CInt
, pgDateWDay :: {-# UNPACK #-} !CInt
} deriving Show

instance Storable PGdate where
  sizeOf _ = #{size PGdate}
  alignment _ = #{alignment PGdate}
  peek ptr = PGdate
    <$> #{peek PGdate, isbc} ptr
    <*> #{peek PGdate, year} ptr
    <*> #{peek PGdate, mon} ptr
    <*> #{peek PGdate, mday} ptr
    <*> #{peek PGdate, jday} ptr
    <*> #{peek PGdate, yday} ptr
    <*> #{peek PGdate, wday} ptr
  poke ptr PGdate{..} = do
    #{poke PGdate, isbc} ptr pgDateIsBC
    #{poke PGdate, year} ptr pgDateYear
    #{poke PGdate, mon} ptr pgDateMon
    #{poke PGdate, mday} ptr pgDateMDay
    #{poke PGdate, jday} ptr pgDateJDay
    #{poke PGdate, yday} ptr pgDateYDay
    #{poke PGdate, wday} ptr pgDateWDay

data PGtime = PGtime {
  pgTimeHour   :: {-# UNPACK #-} !CInt
, pgTimeMin    :: {-# UNPACK #-} !CInt
, pgTimeSec    :: {-# UNPACK #-} !CInt
, pgTimeUSec   :: {-# UNPACK #-} !CInt
, pgTimeWithTZ :: {-# UNPACK #-} !CInt
, pgTimeIsDST  :: {-# UNPACK #-} !CInt
, pgTimeGMTOff :: {-# UNPACK #-} !CInt
, pgTimeTZAbbr :: {-# UNPACK #-} !BS.ByteString
} deriving Show

instance Storable PGtime where
  sizeOf _ = #{size PGtime}
  alignment _ = #{alignment PGtime}
  peek ptr = PGtime
    <$> #{peek PGtime, hour} ptr
    <*> #{peek PGtime, min} ptr
    <*> #{peek PGtime, sec} ptr
    <*> #{peek PGtime, usec} ptr
    <*> #{peek PGtime, withtz} ptr
    <*> #{peek PGtime, isdst} ptr
    <*> #{peek PGtime, gmtoff} ptr
    <*> BS.packCString (#{ptr PGtime, tzabbr} ptr)
  poke ptr PGtime{..} = do
    #{poke PGtime, hour} ptr pgTimeHour
    #{poke PGtime, min} ptr pgTimeMin
    #{poke PGtime, sec} ptr pgTimeSec
    #{poke PGtime, usec} ptr pgTimeUSec
    #{poke PGtime, withtz} ptr pgTimeWithTZ
    #{poke PGtime, isdst} ptr pgTimeIsDST
    #{poke PGtime, gmtoff} ptr pgTimeGMTOff
    unsafeUseAsCStringLen pgTimeTZAbbr $ \(cs, len) -> do
      let tzabbr = #{ptr PGtime, tzabbr} ptr
      copyArray tzabbr cs (min len 16)
      pokeElemOff tzabbr (min len 15) (0::CChar)

data PGtimestamp = PGtimestamp {
  pgTimestampEpoch :: {-# UNPACK #-} !CLong
, pgTimestampDate  :: {-# UNPACK #-} !PGdate
, pgTimestampTime  :: {-# UNPACK #-} !PGtime
} deriving Show

instance Storable PGtimestamp where
  sizeOf _ = #{size PGtimestamp}
  alignment _ = #{alignment PGtimestamp}
  peek ptr = PGtimestamp
    <$> #{peek PGtimestamp, epoch} ptr
    <*> #{peek PGtimestamp, date} ptr
    <*> #{peek PGtimestamp, time} ptr
  poke ptr PGtimestamp{..} = do
    #{poke PGtimestamp, epoch} ptr pgTimestampEpoch
    #{poke PGtimestamp, date} ptr pgTimestampDate
    #{poke PGtimestamp, time} ptr pgTimestampTime
