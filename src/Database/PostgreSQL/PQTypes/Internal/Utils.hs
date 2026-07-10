module Database.PostgreSQL.PQTypes.Internal.Utils
  ( MkConstraint
  , mread
  , safePeekCString
  , safePeekCString'
  , throwLibPQError
  , hpqTypesError
  , unexpectedNULL
  ) where

import Control.Exception qualified as E
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
