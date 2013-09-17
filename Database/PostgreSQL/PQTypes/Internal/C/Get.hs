{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Database.PostgreSQL.PQTypes.Internal.C.Get where

import Foreign.C
import Foreign.Ptr

import Database.PostgreSQL.PQTypes.Internal.C.Types

foreign import ccall unsafe "PQgetf"
  c_PQgetf1 :: Ptr PGresult -> CInt -> CString
            -> CInt -> Ptr t1
            -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf2 :: Ptr PGresult -> CInt -> CString
            -> CInt -> Ptr t1 -> CInt -> Ptr t2
            -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf3 :: Ptr PGresult -> CInt -> CString
            -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3
            -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf4 :: Ptr PGresult -> CInt -> CString
            -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
            -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf5 :: Ptr PGresult -> CInt -> CString
            -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
            -> CInt -> Ptr t5
            -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf6 :: Ptr PGresult -> CInt -> CString
            -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
            -> CInt -> Ptr t5 -> CInt -> Ptr t6
            -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf7 :: Ptr PGresult -> CInt -> CString
            -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
            -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7
            -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf8 :: Ptr PGresult -> CInt -> CString
            -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
            -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
            -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf9 :: Ptr PGresult -> CInt -> CString
            -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
            -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
            -> CInt -> Ptr t9 -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf10 :: Ptr PGresult -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10
             -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf11 :: Ptr PGresult -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11
             -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf12 :: Ptr PGresult -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf13 :: Ptr PGresult -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf14 :: Ptr PGresult -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14
             -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf15 :: Ptr PGresult -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15
             -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf16 :: Ptr PGresult -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf17 :: Ptr PGresult -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17
             -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf18 :: Ptr PGresult -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18
             -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf19 :: Ptr PGresult -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19
             -> IO CInt

foreign import ccall unsafe "PQgetf"
  c_PQgetf20 :: Ptr PGresult -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> IO CInt
