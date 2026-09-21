module Database.PostgreSQL.PQTypes.Internal.Utils
  ( MkConstraint
  , isAsyncException
  , catchSync
  , mread
  , safePeekCString
  , safePeekCString'
  , throwLibPQError
  , hpqTypesError
  , unexpectedNULL
  , runCleanup
  ) where

import Control.Exception qualified as E
import Control.Monad.Catch
import Data.Kind (Type)
import Data.Maybe
import Foreign.C
import Foreign.Ptr
import GHC.Exts
import GHC.Stack

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Error

type family
  MkConstraint
    (m :: Type -> Type)
    (cs :: [(Type -> Type) -> Constraint])
    :: Constraint
  where
  MkConstraint m '[] = ()
  MkConstraint m (c ': cs) = (c m, MkConstraint m cs)

-- | Whether an exception is asynchronous.
isAsyncException :: E.SomeException -> Bool
isAsyncException e = case E.fromException e of
  Just E.SomeAsyncException {} -> True
  Nothing -> False

-- | Like 'catch' with a handler for any exception. An asynchronous exception
-- bypasses the handler and propagates.
catchSync :: MonadCatch m => m a -> (E.SomeException -> m a) -> m a
catchSync action handler =
  action `catch` \e ->
    if isAsyncException e
      then throwM e
      else handler e

-- | Run the cleanup of a bracket, given the exit case of the bracketed
-- action.
--
-- If the action succeeded, a failure of the cleanup propagates. If the action
-- failed, its own exception must propagate instead, so a synchronous failure
-- of the cleanup is dropped. Without this, the failure of the cleanup would
-- replace the exception of the action, and e.g. a restart predicate would
-- never see it. This happens in practice: when the connection died, both the
-- action and its cleanup fail. An asynchronous exception delivered during the
-- cleanup still propagates, so that e.g. a thread cancellation is not lost.
runCleanup :: MonadCatch m => ExitCase a -> m () -> m ()
runCleanup ec cleanup = case ec of
  ExitCaseSuccess _ -> cleanup
  _ -> cleanup `catchSync` \_ -> pure ()

-- Safely read value.
mread :: Read a => String -> Maybe a
mread s = do
  [(a, "")] <- Just (reads s)
  Just a

-- | Safely peek C string.
safePeekCString :: CString -> IO (Maybe String)
safePeekCString cs
  | cs == nullPtr = pure Nothing
  | otherwise = Just <$> peekCString cs

-- | Safely peek C string and return "" if NULL.
safePeekCString' :: CString -> IO String
safePeekCString' cs = fromMaybe "" <$> safePeekCString cs

-- | Throw libpq specific error.
throwLibPQError :: HasCallStack => Ptr PGconn -> String -> IO a
throwLibPQError conn ctx = do
  msg <- safePeekCString' =<< c_PQerrorMessage conn
  E.throwIO . LibPQError $
    if null ctx then msg else ctx ++ ": " ++ msg

-- | Throw 'HPQTypesError exception.
hpqTypesError :: HasCallStack => String -> IO a
hpqTypesError = E.throwIO . HPQTypesError

-- | Throw 'unexpected NULL' exception.
unexpectedNULL :: HasCallStack => IO a
unexpectedNULL = hpqTypesError "unexpected NULL"
