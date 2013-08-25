{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module DB.Primitive.Get where

import Foreign.C
import Foreign.Ptr

import DB.Primitive.Types

foreign import ccall unsafe "PQgetf"
  pqGet1 :: Ptr PGresult -> CInt -> CString
         -> CInt -> Ptr t1
         -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet2 :: Ptr PGresult -> CInt -> CString
         -> CInt -> Ptr t1 -> CInt -> Ptr t2
         -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet3 :: Ptr PGresult -> CInt -> CString
         -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3
         -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet4 :: Ptr PGresult -> CInt -> CString
         -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
         -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet5 :: Ptr PGresult -> CInt -> CString
         -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
         -> CInt -> Ptr t5
         -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet6 :: Ptr PGresult -> CInt -> CString
         -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
         -> CInt -> Ptr t5 -> CInt -> Ptr t6
         -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet7 :: Ptr PGresult -> CInt -> CString
         -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
         -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7
         -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet8 :: Ptr PGresult -> CInt -> CString
         -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
         -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
         -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet9 :: Ptr PGresult -> CInt -> CString
         -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
         -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
         -> CInt -> Ptr t9 -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet10 :: Ptr PGresult -> CInt -> CString
          -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
          -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
          -> CInt -> Ptr t9 -> CInt -> Ptr t10
          -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet11 :: Ptr PGresult -> CInt -> CString
          -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
          -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
          -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11
          -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet12 :: Ptr PGresult -> CInt -> CString
          -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
          -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
          -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
          -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet13 :: Ptr PGresult -> CInt -> CString
          -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
          -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
          -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
          -> CInt -> Ptr t13 -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet14 :: Ptr PGresult -> CInt -> CString
          -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
          -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
          -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
          -> CInt -> Ptr t13 -> CInt -> Ptr t14
          -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet15 :: Ptr PGresult -> CInt -> CString
          -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
          -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
          -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
          -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15
          -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet16 :: Ptr PGresult -> CInt -> CString
          -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
          -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
          -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
          -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
          -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet17 :: Ptr PGresult -> CInt -> CString
          -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
          -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
          -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
          -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
          -> CInt -> Ptr t17
          -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet18 :: Ptr PGresult -> CInt -> CString
          -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
          -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
          -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
          -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
          -> CInt -> Ptr t17 -> CInt -> Ptr t18
          ->IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet19 :: Ptr PGresult -> CInt -> CString
          -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
          -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
          -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
          -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
          -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19
          -> IO CInt

foreign import ccall unsafe "PQgetf"
  pqGet20 :: Ptr PGresult -> CInt -> CString
          -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
          -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
          -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
          -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
          -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
          -> IO CInt
