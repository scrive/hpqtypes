{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | Exports a set of FFI-imported PQputf functions with different arities
-- (PQputf is a variadic function and there is no way to import such
-- functions with FFI in their most generic form).
module Database.PostgreSQL.PQTypes.Internal.C.Put where

import Foreign.C
import Foreign.Ptr

import Database.PostgreSQL.PQTypes.Internal.C.Types

foreign import ccall unsafe "PQputf"
  c_PQputf1 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t
            -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf2 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t1 -> Ptr t2
            -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf3 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t1 -> Ptr t2 -> Ptr t3
            -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf4 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4
            -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf5 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5
            -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf6 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6
            -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf7 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
            -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf8 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
            -> Ptr t8
            -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf9 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
            -> Ptr t8 -> Ptr t9
            -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf10 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10
             -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf11 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11
             -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf12 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12
             -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf13 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13
             -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf14 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf15 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15
             -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf16 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16
             -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf17 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17
             -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf18 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18
             -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf19 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19
             -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputf20 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20
             -> IO CInt
