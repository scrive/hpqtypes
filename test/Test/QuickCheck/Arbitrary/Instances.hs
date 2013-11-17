{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Test.QuickCheck.Arbitrary.Instances where

import Control.Applicative
import Test.QuickCheck.Arbitrary

instance (
    Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5
  , Arbitrary a6
  ) => Arbitrary (a1, a2, a3, a4, a5, a6) where
    arbitrary = (,,,,,)
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary

instance (
    Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5
  , Arbitrary a6, Arbitrary a7
  ) => Arbitrary (a1, a2, a3, a4, a5, a6, a7) where
    arbitrary = (,,,,,,)
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary

instance (
    Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5
  , Arbitrary a6, Arbitrary a7, Arbitrary a8
  ) => Arbitrary (a1, a2, a3, a4, a5, a6, a7, a8) where
    arbitrary = (,,,,,,,)
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary

instance (
    Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5
  , Arbitrary a6, Arbitrary a7, Arbitrary a8, Arbitrary a9
  ) => Arbitrary (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    arbitrary = (,,,,,,,,)
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (
    Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5
  , Arbitrary a6, Arbitrary a7, Arbitrary a8, Arbitrary a9, Arbitrary a10
  ) => Arbitrary (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    arbitrary = (,,,,,,,,,)
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (
    Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5
  , Arbitrary a6, Arbitrary a7, Arbitrary a8, Arbitrary a9, Arbitrary a10
  , Arbitrary a11
  ) => Arbitrary (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    arbitrary = (,,,,,,,,,,)
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary

instance (
    Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5
  , Arbitrary a6, Arbitrary a7, Arbitrary a8, Arbitrary a9, Arbitrary a10
  , Arbitrary a11, Arbitrary a12
  ) => Arbitrary (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    arbitrary = (,,,,,,,,,,,)
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary

instance (
    Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5
  , Arbitrary a6, Arbitrary a7, Arbitrary a8, Arbitrary a9, Arbitrary a10
  , Arbitrary a11, Arbitrary a12, Arbitrary a13
  ) => Arbitrary (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    arbitrary = (,,,,,,,,,,,,)
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary

instance (
    Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5
  , Arbitrary a6, Arbitrary a7, Arbitrary a8, Arbitrary a9, Arbitrary a10
  , Arbitrary a11, Arbitrary a12, Arbitrary a13, Arbitrary a14
  ) => Arbitrary (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    arbitrary = (,,,,,,,,,,,,,)
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (
    Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5
  , Arbitrary a6, Arbitrary a7, Arbitrary a8, Arbitrary a9, Arbitrary a10
  , Arbitrary a11, Arbitrary a12, Arbitrary a13, Arbitrary a14, Arbitrary a15
  ) => Arbitrary (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    arbitrary = (,,,,,,,,,,,,,,)
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (
    Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5
  , Arbitrary a6, Arbitrary a7, Arbitrary a8, Arbitrary a9, Arbitrary a10
  , Arbitrary a11, Arbitrary a12, Arbitrary a13, Arbitrary a14, Arbitrary a15
  , Arbitrary a16
  ) => Arbitrary (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) where
    arbitrary = (,,,,,,,,,,,,,,,)
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary

instance (
    Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5
  , Arbitrary a6, Arbitrary a7, Arbitrary a8, Arbitrary a9, Arbitrary a10
  , Arbitrary a11, Arbitrary a12, Arbitrary a13, Arbitrary a14, Arbitrary a15
  , Arbitrary a16, Arbitrary a17
  ) => Arbitrary (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) where
    arbitrary = (,,,,,,,,,,,,,,,,)
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary

instance (
    Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5
  , Arbitrary a6, Arbitrary a7, Arbitrary a8, Arbitrary a9, Arbitrary a10
  , Arbitrary a11, Arbitrary a12, Arbitrary a13, Arbitrary a14, Arbitrary a15
  , Arbitrary a16, Arbitrary a17, Arbitrary a18
  ) => Arbitrary (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) where
    arbitrary = (,,,,,,,,,,,,,,,,,)
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary

instance (
    Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5
  , Arbitrary a6, Arbitrary a7, Arbitrary a8, Arbitrary a9, Arbitrary a10
  , Arbitrary a11, Arbitrary a12, Arbitrary a13, Arbitrary a14, Arbitrary a15
  , Arbitrary a16, Arbitrary a17, Arbitrary a18, Arbitrary a19
  ) => Arbitrary (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) where
    arbitrary = (,,,,,,,,,,,,,,,,,,)
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (
    Arbitrary a1, Arbitrary a2, Arbitrary a3, Arbitrary a4, Arbitrary a5
  , Arbitrary a6, Arbitrary a7, Arbitrary a8, Arbitrary a9, Arbitrary a10
  , Arbitrary a11, Arbitrary a12, Arbitrary a13, Arbitrary a14, Arbitrary a15
  , Arbitrary a16, Arbitrary a17, Arbitrary a18, Arbitrary a19, Arbitrary a20
  ) => Arbitrary (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) where
    arbitrary = (,,,,,,,,,,,,,,,,,,,)
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
