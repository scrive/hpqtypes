-- | Exports a set of FFI-imported PQgetf functions with different arities
-- (PQgetf is a variadic function and there is no way to import such
-- functions with FFI in their most generic form).
module Database.PostgreSQL.PQTypes.Internal.C.Get where

import Foreign.C
import Foreign.Ptr

import Database.PostgreSQL.PQTypes.Internal.C.Types

-- For PQgetf, we use capi imports since ccall is broken for variadic functions.
-- See https://www.haskell.org/ghc/blog/20210709-capi-usage.html.

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf1 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
            -> CInt -> Ptr t1
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf2 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
            -> CInt -> Ptr t1 -> CInt -> Ptr t2
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf3 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
            -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf4 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
            -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf5 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
            -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
            -> CInt -> Ptr t5
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf6 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
            -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
            -> CInt -> Ptr t5 -> CInt -> Ptr t6
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf7 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
            -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
            -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf8 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
            -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
            -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf9 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
            -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
            -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
            -> CInt -> Ptr t9 -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf10 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf11 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf12 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf13 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf14 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf15 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf16 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf17 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf18 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf19 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf20 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf21 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf22 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf23 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf24 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf25 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf26 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf27 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf28 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf29 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf30 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf31 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf32 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf33 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf34 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf35 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34 -> CInt -> Ptr t35
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf36 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34 -> CInt -> Ptr t35 -> CInt -> Ptr t36
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf37 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34 -> CInt -> Ptr t35 -> CInt -> Ptr t36
             -> CInt -> Ptr t37
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf38 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34 -> CInt -> Ptr t35 -> CInt -> Ptr t36
             -> CInt -> Ptr t37 -> CInt -> Ptr t38
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf39 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34 -> CInt -> Ptr t35 -> CInt -> Ptr t36
             -> CInt -> Ptr t37 -> CInt -> Ptr t38 -> CInt -> Ptr t39
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf40 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34 -> CInt -> Ptr t35 -> CInt -> Ptr t36
             -> CInt -> Ptr t37 -> CInt -> Ptr t38 -> CInt -> Ptr t39 -> CInt -> Ptr t40
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf41 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34 -> CInt -> Ptr t35 -> CInt -> Ptr t36
             -> CInt -> Ptr t37 -> CInt -> Ptr t38 -> CInt -> Ptr t39 -> CInt -> Ptr t40
             -> CInt -> Ptr t41
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf42 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34 -> CInt -> Ptr t35 -> CInt -> Ptr t36
             -> CInt -> Ptr t37 -> CInt -> Ptr t38 -> CInt -> Ptr t39 -> CInt -> Ptr t40
             -> CInt -> Ptr t41 -> CInt -> Ptr t42
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf43 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34 -> CInt -> Ptr t35 -> CInt -> Ptr t36
             -> CInt -> Ptr t37 -> CInt -> Ptr t38 -> CInt -> Ptr t39 -> CInt -> Ptr t40
             -> CInt -> Ptr t41 -> CInt -> Ptr t42 -> CInt -> Ptr t43
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf44 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34 -> CInt -> Ptr t35 -> CInt -> Ptr t36
             -> CInt -> Ptr t37 -> CInt -> Ptr t38 -> CInt -> Ptr t39 -> CInt -> Ptr t40
             -> CInt -> Ptr t41 -> CInt -> Ptr t42 -> CInt -> Ptr t43 -> CInt -> Ptr t44
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf45 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34 -> CInt -> Ptr t35 -> CInt -> Ptr t36
             -> CInt -> Ptr t37 -> CInt -> Ptr t38 -> CInt -> Ptr t39 -> CInt -> Ptr t40
             -> CInt -> Ptr t41 -> CInt -> Ptr t42 -> CInt -> Ptr t43 -> CInt -> Ptr t44
             -> CInt -> Ptr t45
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf46 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34 -> CInt -> Ptr t35 -> CInt -> Ptr t36
             -> CInt -> Ptr t37 -> CInt -> Ptr t38 -> CInt -> Ptr t39 -> CInt -> Ptr t40
             -> CInt -> Ptr t41 -> CInt -> Ptr t42 -> CInt -> Ptr t43 -> CInt -> Ptr t44
             -> CInt -> Ptr t45 -> CInt -> Ptr t46
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf47 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34 -> CInt -> Ptr t35 -> CInt -> Ptr t36
             -> CInt -> Ptr t37 -> CInt -> Ptr t38 -> CInt -> Ptr t39 -> CInt -> Ptr t40
             -> CInt -> Ptr t41 -> CInt -> Ptr t42 -> CInt -> Ptr t43 -> CInt -> Ptr t44
             -> CInt -> Ptr t45 -> CInt -> Ptr t46 -> CInt -> Ptr t47
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf48 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34 -> CInt -> Ptr t35 -> CInt -> Ptr t36
             -> CInt -> Ptr t37 -> CInt -> Ptr t38 -> CInt -> Ptr t39 -> CInt -> Ptr t40
             -> CInt -> Ptr t41 -> CInt -> Ptr t42 -> CInt -> Ptr t43 -> CInt -> Ptr t44
             -> CInt -> Ptr t45 -> CInt -> Ptr t46 -> CInt -> Ptr t47 -> CInt -> Ptr t48
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf49 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34 -> CInt -> Ptr t35 -> CInt -> Ptr t36
             -> CInt -> Ptr t37 -> CInt -> Ptr t38 -> CInt -> Ptr t39 -> CInt -> Ptr t40
             -> CInt -> Ptr t41 -> CInt -> Ptr t42 -> CInt -> Ptr t43 -> CInt -> Ptr t44
             -> CInt -> Ptr t45 -> CInt -> Ptr t46 -> CInt -> Ptr t47 -> CInt -> Ptr t48
             -> CInt -> Ptr t49
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQgetf"
  c_PQgetf50 :: Ptr PGresult -> Ptr PGerror -> CInt -> CString
             -> CInt -> Ptr t1 -> CInt -> Ptr t2 -> CInt -> Ptr t3 -> CInt -> Ptr t4
             -> CInt -> Ptr t5 -> CInt -> Ptr t6 -> CInt -> Ptr t7 -> CInt -> Ptr t8
             -> CInt -> Ptr t9 -> CInt -> Ptr t10 -> CInt -> Ptr t11 -> CInt -> Ptr t12
             -> CInt -> Ptr t13 -> CInt -> Ptr t14 -> CInt -> Ptr t15 -> CInt -> Ptr t16
             -> CInt -> Ptr t17 -> CInt -> Ptr t18 -> CInt -> Ptr t19 -> CInt -> Ptr t20
             -> CInt -> Ptr t21 -> CInt -> Ptr t22 -> CInt -> Ptr t23 -> CInt -> Ptr t24
             -> CInt -> Ptr t25 -> CInt -> Ptr t26 -> CInt -> Ptr t27 -> CInt -> Ptr t28
             -> CInt -> Ptr t29 -> CInt -> Ptr t30 -> CInt -> Ptr t31 -> CInt -> Ptr t32
             -> CInt -> Ptr t33 -> CInt -> Ptr t34 -> CInt -> Ptr t35 -> CInt -> Ptr t36
             -> CInt -> Ptr t37 -> CInt -> Ptr t38 -> CInt -> Ptr t39 -> CInt -> Ptr t40
             -> CInt -> Ptr t41 -> CInt -> Ptr t42 -> CInt -> Ptr t43 -> CInt -> Ptr t44
             -> CInt -> Ptr t45 -> CInt -> Ptr t46 -> CInt -> Ptr t47 -> CInt -> Ptr t48
             -> CInt -> Ptr t49 -> CInt -> Ptr t50
             -> IO CInt
