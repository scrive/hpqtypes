module Database.PostgreSQL.PQTypes.Internal.BackendPid
  ( BackendPid (..)
  , noBackendPid
  ) where

-- | Process ID of the server process attached to the current session.
newtype BackendPid = BackendPid Int
  deriving newtype (Eq, Ord, Show)

noBackendPid :: BackendPid
noBackendPid = BackendPid 0
