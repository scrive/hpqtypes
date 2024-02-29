module Database.PostgreSQL.PQTypes.Internal.Notification (
    Channel(..)
  , Notification(..)
  , getNotificationIO
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Data.String
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Types
import System.Timeout
import Control.Exception qualified as E
import Data.ByteString.Char8 qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.SQL.Raw

#include <libpq-fe.h>

foreign import ccall unsafe "PQnotifies"
  c_PQnotifies :: Ptr PGconn -> IO (Ptr Notification)

----------------------------------------

-- | Representation of notification channel.
newtype Channel = Channel (RawSQL ())
  deriving (Eq, Ord)

instance IsString Channel where
  fromString = Channel . fromString

instance Show Channel where
  showsPrec n (Channel chan) = ("Channel " ++) . showsPrec n (unRawSQL chan)

----------------------------------------

-- | Representation of a notification sent by PostgreSQL.
data Notification = Notification
  { -- | Process ID of notifying server.
    ntPID     :: !CPid
    -- | Notification channel name.
  , ntChannel :: !Channel
    -- | Notification payload string.
  , ntPayload :: !T.Text
  } deriving (Eq, Ord, Show)

instance Storable Notification where
  sizeOf _ = #{size PGnotify}
  alignment _ = #{alignment PGnotify}
  peek ptr = do
    ntPID <- return . CPid
      =<< #{peek PGnotify, be_pid} ptr
    ntChannel <- fmap (Channel . flip rawSQL () . T.decodeUtf8) . BS.packCString
      =<< #{peek PGnotify, relname} ptr
    ntPayload <- fmap T.decodeUtf8 . BS.packCString
      =<< #{peek PGnotify, extra} ptr
    return Notification{..}
  poke _ _ = error "Storable Notification: poke is not supposed to be used"

----------------------------------------

-- | Low-level function that waits for a notification for a given
-- number of microseconds (it uses 'timeout' function internally).
getNotificationIO :: DBState m -> Int -> IO (Maybe Notification)
getNotificationIO st n = timeout n $ do
  withConnectionData (dbConnection st) fname $ \cd -> fix $ \loop -> do
    let conn = cdPtr cd
    mmsg <- tryGet conn
    case mmsg of
      Just msg -> return (cd, msg)
      Nothing -> do
        fd <- c_PQsocket conn
        if fd == -1
          then hpqTypesError $ fname ++ ": invalid file descriptor"
          else do
            threadWaitRead fd
            res <- c_PQconsumeInput conn
            when (res /= 1) $ do
              throwLibPQError conn fname
            loop
  where
    fname :: String
    fname = "getNotificationIO"

    tryGet :: Ptr PGconn -> IO (Maybe Notification)
    tryGet connPtr = E.mask_ $ do
      ptr <- c_PQnotifies connPtr
      if ptr /= nullPtr
        then do
          msg <- peek ptr
          c_PQfreemem ptr
          return $ Just msg
        else return Nothing
