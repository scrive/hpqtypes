{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Prelude.Instances where

instance (
    Eq a1, Eq a2, Eq a3, Eq a4, Eq a5, Eq a6, Eq a7, Eq a8, Eq a9, Eq a10
  , Eq a11, Eq a12, Eq a13, Eq a14, Eq a15, Eq a16
  ) => Eq (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) where
    (==)
      (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
      (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16) = and [
        a1 == b1, a2 == b2, a3 == b3, a4 == b4, a5 == b5, a6 == b6, a7 == b7
      , a8 == b8, a9 == b9, a10 == b10, a11 == b11, a12 == b12, a13 == b13, a14 == b14
      , a15 == b15, a16 == b16
      ]

instance (
    Eq a1, Eq a2, Eq a3, Eq a4, Eq a5, Eq a6, Eq a7, Eq a8, Eq a9, Eq a10
  , Eq a11, Eq a12, Eq a13, Eq a14, Eq a15, Eq a16, Eq a17
  ) => Eq (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) where
    (==)
      (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
      (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17) = and [
        a1 == b1, a2 == b2, a3 == b3, a4 == b4, a5 == b5, a6 == b6, a7 == b7
      , a8 == b8, a9 == b9, a10 == b10, a11 == b11, a12 == b12, a13 == b13, a14 == b14
      , a15 == b15, a16 == b16, a17 == b17
      ]

instance (
    Eq a1, Eq a2, Eq a3, Eq a4, Eq a5, Eq a6, Eq a7, Eq a8, Eq a9, Eq a10
  , Eq a11, Eq a12, Eq a13, Eq a14, Eq a15, Eq a16, Eq a17, Eq a18
  ) => Eq (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) where
    (==)
      (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
      (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18) = and [
        a1 == b1, a2 == b2, a3 == b3, a4 == b4, a5 == b5, a6 == b6, a7 == b7
      , a8 == b8, a9 == b9, a10 == b10, a11 == b11, a12 == b12, a13 == b13, a14 == b14
      , a15 == b15, a16 == b16, a17 == b17, a18 == b18
      ]

instance (
    Eq a1, Eq a2, Eq a3, Eq a4, Eq a5, Eq a6, Eq a7, Eq a8, Eq a9, Eq a10
  , Eq a11, Eq a12, Eq a13, Eq a14, Eq a15, Eq a16, Eq a17, Eq a18, Eq a19
  ) => Eq (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) where
    (==)
      (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
      (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19) = and [
        a1 == b1, a2 == b2, a3 == b3, a4 == b4, a5 == b5, a6 == b6, a7 == b7
      , a8 == b8, a9 == b9, a10 == b10, a11 == b11, a12 == b12, a13 == b13, a14 == b14
      , a15 == b15, a16 == b16, a17 == b17, a18 == b18, a19 == b19
      ]

instance (
    Eq a1, Eq a2, Eq a3, Eq a4, Eq a5, Eq a6, Eq a7, Eq a8, Eq a9, Eq a10
  , Eq a11, Eq a12, Eq a13, Eq a14, Eq a15, Eq a16, Eq a17, Eq a18, Eq a19, Eq a20
  ) => Eq (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) where
    (==)
      (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
      (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20) = and [
        a1 == b1, a2 == b2, a3 == b3, a4 == b4, a5 == b5, a6 == b6, a7 == b7
      , a8 == b8, a9 == b9, a10 == b10, a11 == b11, a12 == b12, a13 == b13, a14 == b14
      , a15 == b15, a16 == b16, a17 == b17, a18 == b18, a19 == b19, a20 == b20
      ]

instance (
    Show a1, Show a2, Show a3, Show a4, Show a5, Show a6, Show a7, Show a8
  , Show a9, Show a10, Show a11, Show a12, Show a13, Show a14, Show a15, Show a16
  ) => Show (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) where
    showsPrec n (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) = ("(" ++)
      . showsPrec n a1  . sep . showsPrec n a2  . sep . showsPrec n a3  . sep
      . showsPrec n a4  . sep . showsPrec n a5  . sep . showsPrec n a6  . sep
      . showsPrec n a7  . sep . showsPrec n a8  . sep . showsPrec n a9  . sep
      . showsPrec n a10 . sep . showsPrec n a11 . sep . showsPrec n a12 . sep
      . showsPrec n a13 . sep . showsPrec n a14 . sep . showsPrec n a15 . sep
      . showsPrec n a16
      . (")" ++)
      where
        sep = ("," ++)

instance (
    Show a1, Show a2, Show a3, Show a4, Show a5, Show a6, Show a7, Show a8
  , Show a9, Show a10, Show a11, Show a12, Show a13, Show a14, Show a15, Show a16
  , Show a17
  ) => Show (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) where
    showsPrec n (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) = ("(" ++)
      . showsPrec n a1  . sep . showsPrec n a2  . sep . showsPrec n a3  . sep
      . showsPrec n a4  . sep . showsPrec n a5  . sep . showsPrec n a6  . sep
      . showsPrec n a7  . sep . showsPrec n a8  . sep . showsPrec n a9  . sep
      . showsPrec n a10 . sep . showsPrec n a11 . sep . showsPrec n a12 . sep
      . showsPrec n a13 . sep . showsPrec n a14 . sep . showsPrec n a15 . sep
      . showsPrec n a16 . sep . showsPrec n a17
      . (")" ++)
      where
        sep = ("," ++)

instance (
    Show a1, Show a2, Show a3, Show a4, Show a5, Show a6, Show a7, Show a8
  , Show a9, Show a10, Show a11, Show a12, Show a13, Show a14, Show a15, Show a16
  , Show a17, Show a18
  ) => Show (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) where
    showsPrec n (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) = ("(" ++)
      . showsPrec n a1  . sep . showsPrec n a2  . sep . showsPrec n a3  . sep
      . showsPrec n a4  . sep . showsPrec n a5  . sep . showsPrec n a6  . sep
      . showsPrec n a7  . sep . showsPrec n a8  . sep . showsPrec n a9  . sep
      . showsPrec n a10 . sep . showsPrec n a11 . sep . showsPrec n a12 . sep
      . showsPrec n a13 . sep . showsPrec n a14 . sep . showsPrec n a15 . sep
      . showsPrec n a16 . sep . showsPrec n a17 . sep . showsPrec n a18
      . (")" ++)
      where
        sep = ("," ++)

instance (
    Show a1, Show a2, Show a3, Show a4, Show a5, Show a6, Show a7, Show a8
  , Show a9, Show a10, Show a11, Show a12, Show a13, Show a14, Show a15, Show a16
  , Show a17, Show a18, Show a19
  ) => Show (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) where
    showsPrec n (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) = ("(" ++)
      . showsPrec n a1  . sep . showsPrec n a2  . sep . showsPrec n a3  . sep
      . showsPrec n a4  . sep . showsPrec n a5  . sep . showsPrec n a6  . sep
      . showsPrec n a7  . sep . showsPrec n a8  . sep . showsPrec n a9  . sep
      . showsPrec n a10 . sep . showsPrec n a11 . sep . showsPrec n a12 . sep
      . showsPrec n a13 . sep . showsPrec n a14 . sep . showsPrec n a15 . sep
      . showsPrec n a16 . sep . showsPrec n a17 . sep . showsPrec n a18 . sep
      . showsPrec n a19
      . (")" ++)
      where
        sep = ("," ++)

instance (
    Show a1, Show a2, Show a3, Show a4, Show a5, Show a6, Show a7, Show a8
  , Show a9, Show a10, Show a11, Show a12, Show a13, Show a14, Show a15, Show a16
  , Show a17, Show a18, Show a19, Show a20
  ) => Show (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) where
    showsPrec n (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) = ("(" ++)
      . showsPrec n a1  . sep . showsPrec n a2  . sep . showsPrec n a3  . sep
      . showsPrec n a4  . sep . showsPrec n a5  . sep . showsPrec n a6  . sep
      . showsPrec n a7  . sep . showsPrec n a8  . sep . showsPrec n a9  . sep
      . showsPrec n a10 . sep . showsPrec n a11 . sep . showsPrec n a12 . sep
      . showsPrec n a13 . sep . showsPrec n a14 . sep . showsPrec n a15 . sep
      . showsPrec n a16 . sep . showsPrec n a17 . sep . showsPrec n a18 . sep
      . showsPrec n a19 . sep . showsPrec n a20
      . (")" ++)
      where
        sep = ("," ++)
