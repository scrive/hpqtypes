{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Database.PostgreSQL.PQTypes.Internal.C.Put where

import Foreign.C
import Foreign.Ptr

import Database.PostgreSQL.PQTypes.Internal.C.Types

foreign import ccall unsafe "PQputf"
  c_PQputf1 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t
            -> IO CInt
