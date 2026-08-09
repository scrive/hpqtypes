module Main (main) where

import Control.DeepSeq
import Data.ByteString qualified as BS
import Data.Int
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Vector qualified as V
import Test.Tasty.Bench

import Database.PostgreSQL.PQTypes

main :: IO ()
main =
  defaultMain
    [ bgroup
        "encode"
        [ bench "Int64" $ nf paramBytes (maxBound :: Int64)
        , bench "Double" $ nf paramBytes (pi :: Double)
        , bench "Text" $ nf paramBytes ("a rather short text" :: Text)
        , bench "UTCTime" $ nf paramBytes (UTCTime (fromGregorian 2026 7 17) 0)
        , bench "[Int32]" $ nf paramBytes ints1
        , bench "[Maybe Int32]" $ nf paramBytes (map Just ints1)
        , bench "[[Int32]]" $ nf paramBytes ints2
        , bench "[[[Int32]]]" $ nf paramBytes ints3
        , bench "[Text]" $ nf paramBytes texts
        , bench "Vector Int32" $ nf paramBytes (V.fromList ints1)
        , bench "Vector (Vector Int32)" $
            nf paramBytes (V.fromList $ map V.fromList ints2)
        , bench "Vector [Int32]" $ nf paramBytes (V.fromList ints2)
        ]
    , bgroup
        "enum"
        [ bench "encode" $ nf (encodeEnumAs @Int16 @T) T42
        , bench "decode" $ nf (\b -> Map.lookup b (enumEncodingMap @Int16 @T)) 42
        ]
    , bgroup
        "enum-text"
        [ bench "encode" $ nf (encodeEnumAs @Text @S) S42
        , bench "decode" $ nf (\t -> Map.lookup t (enumEncodingMap @Text @S)) "text_42"
        ]
    ]
  where
    -- Extract the encoded payload of a query parameter, so that the
    -- benchmark covers the whole encoding pipeline, including serialization
    -- of the 'Encoding' builder into bytes.
    paramBytes :: ToSQL a => a -> Maybe BS.ByteString
    paramBytes v = case toPQParam v of
      PQParam _ payload -> payload

    -- Arrays of every dimensionality have 10000 elements, so that their
    -- encoding times are comparable.
    ints1 :: [Int32]
    ints1 = [1 .. 10000]

    ints2 :: [[Int32]]
    ints2 = groupsOf 100 ints1

    ints3 :: [[[Int32]]]
    ints3 = groupsOf 10 ints2

    texts :: [Text]
    texts = map (T.pack . show) [1 .. 1000 :: Int]

    groupsOf :: Int -> [a] -> [[a]]
    groupsOf n = \case
      [] -> []
      xs -> let (group, rest) = splitAt n xs in group : groupsOf n rest

----------------------------------------

data T
  = T01
  | T02
  | T03
  | T04
  | T05
  | T06
  | T07
  | T08
  | T09
  | T10
  | T11
  | T12
  | T13
  | T14
  | T15
  | T16
  | T17
  | T18
  | T19
  | T20
  | T21
  | T22
  | T23
  | T24
  | T25
  | T26
  | T27
  | T28
  | T29
  | T30
  | T31
  | T32
  | T33
  | T34
  | T35
  | T36
  | T37
  | T38
  | T39
  | T40
  | T41
  | T42
  | T43
  | T44
  | T45
  | T46
  | T47
  | T48
  | T49
  | T50
  deriving stock (Eq, Show, Enum, Bounded)

instance NFData T where
  rnf = (`seq` ())

-- | Enum encoding for 'T'.
--
-- >>> isInjective (encodeEnumAs @Int16 @T)
-- True
instance EnumEncodingAs Int16 T where
  encodeEnumAs = \case
    T01 -> 1
    T02 -> 2
    T03 -> 3
    T04 -> 4
    T05 -> 5
    T06 -> 6
    T07 -> 7
    T08 -> 8
    T09 -> 9
    T10 -> 10
    T11 -> 11
    T12 -> 12
    T13 -> 13
    T14 -> 14
    T15 -> 15
    T16 -> 16
    T17 -> 17
    T18 -> 18
    T19 -> 19
    T20 -> 20
    T21 -> 21
    T22 -> 22
    T23 -> 23
    T24 -> 24
    T25 -> 25
    T26 -> 26
    T27 -> 27
    T28 -> 28
    T29 -> 29
    T30 -> 30
    T31 -> 31
    T32 -> 32
    T33 -> 33
    T34 -> 34
    T35 -> 35
    T36 -> 36
    T37 -> 37
    T38 -> 38
    T39 -> 39
    T40 -> 40
    T41 -> 41
    T42 -> 42
    T43 -> 43
    T44 -> 44
    T45 -> 45
    T46 -> 46
    T47 -> 47
    T48 -> 48
    T49 -> 49
    T50 -> 50

----------------------------------------

data S
  = S01
  | S02
  | S03
  | S04
  | S05
  | S06
  | S07
  | S08
  | S09
  | S10
  | S11
  | S12
  | S13
  | S14
  | S15
  | S16
  | S17
  | S18
  | S19
  | S20
  | S21
  | S22
  | S23
  | S24
  | S25
  | S26
  | S27
  | S28
  | S29
  | S30
  | S31
  | S32
  | S33
  | S34
  | S35
  | S36
  | S37
  | S38
  | S39
  | S40
  | S41
  | S42
  | S43
  | S44
  | S45
  | S46
  | S47
  | S48
  | S49
  | S50
  deriving stock (Eq, Show, Enum, Bounded)

instance NFData S where
  rnf = (`seq` ())

-- | Enum encoding for 'S'.
--
-- >>> isInjective (encodeEnumAs @Text @S)
-- True
instance EnumEncodingAs Text S where
  encodeEnumAs = \case
    S01 -> "text_01"
    S02 -> "text_02"
    S03 -> "text_03"
    S04 -> "text_04"
    S05 -> "text_05"
    S06 -> "text_06"
    S07 -> "text_07"
    S08 -> "text_08"
    S09 -> "text_09"
    S10 -> "text_10"
    S11 -> "text_11"
    S12 -> "text_12"
    S13 -> "text_13"
    S14 -> "text_14"
    S15 -> "text_15"
    S16 -> "text_16"
    S17 -> "text_17"
    S18 -> "text_18"
    S19 -> "text_19"
    S20 -> "text_20"
    S21 -> "text_21"
    S22 -> "text_22"
    S23 -> "text_23"
    S24 -> "text_24"
    S25 -> "text_25"
    S26 -> "text_26"
    S27 -> "text_27"
    S28 -> "text_28"
    S29 -> "text_29"
    S30 -> "text_30"
    S31 -> "text_31"
    S32 -> "text_32"
    S33 -> "text_33"
    S34 -> "text_34"
    S35 -> "text_35"
    S36 -> "text_36"
    S37 -> "text_37"
    S38 -> "text_38"
    S39 -> "text_39"
    S40 -> "text_40"
    S41 -> "text_41"
    S42 -> "text_42"
    S43 -> "text_43"
    S44 -> "text_44"
    S45 -> "text_45"
    S46 -> "text_46"
    S47 -> "text_47"
    S48 -> "text_48"
    S49 -> "text_49"
    S50 -> "text_50"
