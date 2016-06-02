module Database.PostgreSQL.PQTypes.Notification (
    Channel(..)
  , Notification(..)
  , listen
  , unlisten
  , unlistenAll
  , notify
  ) where

import Data.ByteString (ByteString)
import Prelude

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Internal.Notification
import Database.PostgreSQL.PQTypes.SQL.Raw
import Database.PostgreSQL.PQTypes.Utils

-- | Start listening for notifications on a given channel.
listen :: MonadDB m => Channel -> m ()
listen (Channel chan) = runQuery_ $ "LISTEN" <+> chan

-- | Stop listening for notifications on a given channel.
unlisten :: MonadDB m => Channel -> m ()
unlisten (Channel chan) = runQuery_ $ "UNLISTEN" <+> chan

-- | Cancel all listener registrations for the current session.
unlistenAll :: MonadDB m => m ()
unlistenAll = runSQL_ "UNLISTEN *"

-- | Generate a notification on a given channel.
notify :: MonadDB m => Channel -> ByteString -> m ()
notify (Channel chan) payload = runQuery_
  $ rawSQL "SELECT pg_notify($1, $2)" (unRawSQL chan, payload)
