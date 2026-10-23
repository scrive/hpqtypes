module Database.PostgreSQL.PQTypes.Model.ColumnType
  ( ColumnType (..)
  , columnTypeToSQL
  ) where

import Data.Attoparsec.Text qualified as A
import Data.Functor
import Data.Text qualified as T
import Database.PostgreSQL.PQTypes
import TextShow

data ColumnType
  = BigIntT
  | BigSerialT
  | BinaryT
  | BoolT
  | DateT
  | DoubleT
  | IntegerT
  | UuidT
  | IntervalT
  | JsonT
  | JsonbT
  | SmallIntT
  | TextT
  | TimestampWithZoneT
  | TSVectorT
  | XmlT
  | ArrayT !ColumnType
  | CustomT !(RawSQL ())
  | NumericT !(Maybe (Int, Int))
  deriving (Eq, Ord, Show)

instance PQFormat ColumnType where
  pqFormat = pqFormat @T.Text
instance FromSQL ColumnType where
  type PQBase ColumnType = PQBase T.Text
  fromSQL mbase = parseType . T.toLower <$> fromSQL mbase
    where
      parseType :: T.Text -> ColumnType
      parseType = \case
        "bigint" -> BigIntT
        "bytea" -> BinaryT
        "boolean" -> BoolT
        "date" -> DateT
        "double precision" -> DoubleT
        "integer" -> IntegerT
        "uuid" -> UuidT
        "interval" -> IntervalT
        "json" -> JsonT
        "jsonb" -> JsonbT
        "smallint" -> SmallIntT
        "text" -> TextT
        "timestamp with time zone" -> TimestampWithZoneT
        "tsvector" -> TSVectorT
        "xml" -> XmlT
        tname -> case parseNumeric tname of
          Just t -> t
          Nothing
            | "[]" `T.isSuffixOf` tname -> ArrayT . parseType $ T.take (T.length tname - 2) tname
            | otherwise -> CustomT $ rawSQL tname ()
      parseNumeric :: T.Text -> Maybe ColumnType
      parseNumeric tname =
        let inParens p = A.string "(" *> p <* A.string ")"
            comma = A.string "," $> ()
            precisionAndScale = Just <$> inParens ((,) <$> (A.decimal <* comma) <*> A.signed A.decimal)
            precisionOnly = Just . (,0) <$> inParens A.decimal
            numericParser =
              NumericT
                <$> ( A.string "numeric"
                        *> A.choice
                          [ precisionAndScale
                          , precisionOnly
                          , pure Nothing
                          ]
                    )
        in either (const Nothing) Just $ A.parseOnly numericParser tname

columnTypeToSQL :: ColumnType -> RawSQL ()
columnTypeToSQL BigIntT = "BIGINT"
columnTypeToSQL BigSerialT = "BIGSERIAL"
columnTypeToSQL BinaryT = "BYTEA"
columnTypeToSQL BoolT = "BOOLEAN"
columnTypeToSQL DateT = "DATE"
columnTypeToSQL DoubleT = "DOUBLE PRECISION"
columnTypeToSQL IntegerT = "INTEGER"
columnTypeToSQL UuidT = "UUID"
columnTypeToSQL IntervalT = "INTERVAL"
columnTypeToSQL JsonT = "JSON"
columnTypeToSQL JsonbT = "JSONB"
columnTypeToSQL SmallIntT = "SMALLINT"
columnTypeToSQL TextT = "TEXT"
columnTypeToSQL TSVectorT = "TSVECTOR"
columnTypeToSQL TimestampWithZoneT = "TIMESTAMPTZ"
columnTypeToSQL XmlT = "XML"
columnTypeToSQL (ArrayT t) = columnTypeToSQL t <> "[]"
columnTypeToSQL (CustomT tname) = tname
columnTypeToSQL (NumericT Nothing) = rawSQL "NUMERIC" ()
columnTypeToSQL (NumericT (Just (precision, scale))) = rawSQL ("NUMERIC(" <> showt precision <> "," <> showt scale <> ")") ()
