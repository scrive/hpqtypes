{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Database.PostgreSQL.PQTypes.Internal.Composite where

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Utils

registerComposites :: Ptr PGconn -> [String] -> IO ()
registerComposites _ [] = return ()
registerComposites conn names =
  E.bracket (E.mask_ $ mapM newCString names) (mapM_ free) $ \cnames ->
    withArray (map nameToTypeRep cnames) $ \typereps -> do
      let len = fromIntegral $ length cnames
      c_PQregisterTypes conn c_PQT_COMPOSITE typereps len 0
        >>= verifyPQTRes "registerComposites"
  where
    nameToTypeRep name = PGregisterType {
      pgRegisterTypeTypName = name
    , pgRegisterTypeTypPut = nullFunPtr
    , pgRegisterTypeTypGet = nullFunPtr
    }
