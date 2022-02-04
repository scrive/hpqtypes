-- | Exports a set of FFI-imported PQputf functions with different arities
-- (PQputf is a variadic function and there is no way to import such
-- functions with FFI in their most generic form).
module Database.PostgreSQL.PQTypes.Internal.C.Put where

import Foreign.C
import Foreign.Ptr

import Database.PostgreSQL.PQTypes.Internal.C.Types

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf1 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf2 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t1 -> Ptr t2
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf3 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t1 -> Ptr t2 -> Ptr t3
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf4 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf5 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf6 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf7 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf8 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
            -> Ptr t8
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf9 :: Ptr PGparam -> Ptr PGerror -> CString
            -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
            -> Ptr t8 -> Ptr t9
            -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf10 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf11 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf12 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf13 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf14 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf15 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf16 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf17 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf18 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf19 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf20 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf21 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf22 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf23 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf24 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf25 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf26 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf27 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf28 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf29 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf30 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf31 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf32 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf33 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf34 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf35 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34 -> Ptr t35
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf36 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34 -> Ptr t35
             -> Ptr t36
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf37 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34 -> Ptr t35
             -> Ptr t36 -> Ptr t37
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf38 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34 -> Ptr t35
             -> Ptr t36 -> Ptr t37 -> Ptr t38
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf39 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34 -> Ptr t35
             -> Ptr t36 -> Ptr t37 -> Ptr t38 -> Ptr t39
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf40 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34 -> Ptr t35
             -> Ptr t36 -> Ptr t37 -> Ptr t38 -> Ptr t39 -> Ptr t40
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf41 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34 -> Ptr t35
             -> Ptr t36 -> Ptr t37 -> Ptr t38 -> Ptr t39 -> Ptr t40 -> Ptr t41
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf42 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34 -> Ptr t35
             -> Ptr t36 -> Ptr t37 -> Ptr t38 -> Ptr t39 -> Ptr t40 -> Ptr t41 -> Ptr t42
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf43 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34 -> Ptr t35
             -> Ptr t36 -> Ptr t37 -> Ptr t38 -> Ptr t39 -> Ptr t40 -> Ptr t41 -> Ptr t42
             -> Ptr t43
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf44 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34 -> Ptr t35
             -> Ptr t36 -> Ptr t37 -> Ptr t38 -> Ptr t39 -> Ptr t40 -> Ptr t41 -> Ptr t42
             -> Ptr t43 -> Ptr t44
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf45 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34 -> Ptr t35
             -> Ptr t36 -> Ptr t37 -> Ptr t38 -> Ptr t39 -> Ptr t40 -> Ptr t41 -> Ptr t42
             -> Ptr t43 -> Ptr t44 -> Ptr t45
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf46 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34 -> Ptr t35
             -> Ptr t36 -> Ptr t37 -> Ptr t38 -> Ptr t39 -> Ptr t40 -> Ptr t41 -> Ptr t42
             -> Ptr t43 -> Ptr t44 -> Ptr t45 -> Ptr t46
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf47 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34 -> Ptr t35
             -> Ptr t36 -> Ptr t37 -> Ptr t38 -> Ptr t39 -> Ptr t40 -> Ptr t41 -> Ptr t42
             -> Ptr t43 -> Ptr t44 -> Ptr t45 -> Ptr t46 -> Ptr t47
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf48 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34 -> Ptr t35
             -> Ptr t36 -> Ptr t37 -> Ptr t38 -> Ptr t39 -> Ptr t40 -> Ptr t41 -> Ptr t42
             -> Ptr t43 -> Ptr t44 -> Ptr t45 -> Ptr t46 -> Ptr t47 -> Ptr t48
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf49 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34 -> Ptr t35
             -> Ptr t36 -> Ptr t37 -> Ptr t38 -> Ptr t39 -> Ptr t40 -> Ptr t41 -> Ptr t42
             -> Ptr t43 -> Ptr t44 -> Ptr t45 -> Ptr t46 -> Ptr t47 -> Ptr t48 -> Ptr t49
             -> IO CInt

foreign import capi unsafe "libpqtypes.h PQputf"
  c_PQputf50 :: Ptr PGparam -> Ptr PGerror -> CString
             -> Ptr t1 -> Ptr t2 -> Ptr t3 -> Ptr t4 -> Ptr t5 -> Ptr t6 -> Ptr t7
             -> Ptr t8 -> Ptr t9 -> Ptr t10 -> Ptr t11 -> Ptr t12 -> Ptr t13 -> Ptr t14
             -> Ptr t15 -> Ptr t16 -> Ptr t17 -> Ptr t18 -> Ptr t19 -> Ptr t20 -> Ptr t21
             -> Ptr t22 -> Ptr t23 -> Ptr t24 -> Ptr t25 -> Ptr t26 -> Ptr t27 -> Ptr t28
             -> Ptr t29 -> Ptr t30 -> Ptr t31 -> Ptr t32 -> Ptr t33 -> Ptr t34 -> Ptr t35
             -> Ptr t36 -> Ptr t37 -> Ptr t38 -> Ptr t39 -> Ptr t40 -> Ptr t41 -> Ptr t42
             -> Ptr t43 -> Ptr t44 -> Ptr t45 -> Ptr t46 -> Ptr t47 -> Ptr t48 -> Ptr t49
             -> Ptr t50
             -> IO CInt
