{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
module Database.PostgreSQL.PQTypes.SQL.Class (
    SomeSQL(..)
  , IsSQL(..)
  ) where

import Foreign.C.String
import Foreign.Ptr

import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.ToSQL

data SomeSQL = forall sql. IsSQL sql => SomeSQL sql

class Show sql => IsSQL sql where
  someSQL :: sql -> SomeSQL
  withSQL :: sql -> ParamAllocator -> (Ptr PGparam -> CString -> IO r) -> IO r
