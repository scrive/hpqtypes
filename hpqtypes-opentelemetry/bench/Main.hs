module Main (main) where

import Data.Text qualified as T
import Test.Tasty.Bench

import Database.PostgreSQL.PQTypes.OpenTelemetry.QuerySummary

main :: IO ()
main =
  defaultMain
    [ bgroup
        "parseQuerySummary"
        [ summary "typical" typicalQuery
        , summary "cte" cteQuery
        , bgroup "union" [summary (show n) $ unionQuery n | n <- [100, 1000, 10000]]
        , bgroup "values" [summary (show n) $ valuesQuery n | n <- [100, 1000, 10000]]
        , bgroup "nested" [summary (show n) $ nestedQuery n | n <- [10, 100, 1000]]
        ]
    ]
  where
    summary name query = env (pure query) $ bench name . nf (renderQuerySummary . parseQuerySummary)

typicalQuery :: T.Text
typicalQuery =
  "SELECT u.id, u.name, o.total FROM users u JOIN orders o ON o.user_id = u.id \
  \WHERE u.id = $1 AND o.created_at > now() - interval '1 day' \
  \ORDER BY o.created_at DESC LIMIT 10"

cteQuery :: T.Text
cteQuery =
  "WITH recent AS (SELECT * FROM orders WHERE created_at > $1), \
  \totals AS (SELECT user_id, sum(total) AS total FROM recent GROUP BY user_id) \
  \UPDATE users SET total = t.total FROM totals t WHERE users.id = t.user_id \
  \RETURNING users.id"

-- | Many operations, e.g. a generated query.
unionQuery :: Int -> T.Text
unionQuery n =
  T.intercalate " UNION ALL "
    [ "SELECT a, b FROM t" <> T.pack (show i) <> " JOIN u ON u.id = t.id WHERE x = $1"
    | i <- [1 .. n]
    ]

-- | A long query with a single operation, e.g. a bulk insert.
valuesQuery :: Int -> T.Text
valuesQuery n =
  "INSERT INTO t (a, b, c) VALUES "
    <> T.intercalate
      ", "
      [ "($" <> T.pack (show (2 * i - 1)) <> ", $" <> T.pack (show (2 * i)) <> ", 'literal')"
      | i <- [1 .. n]
      ]

-- | Deeply nested subqueries.
nestedQuery :: Int -> T.Text
nestedQuery n =
  T.concat (replicate n "SELECT * FROM (")
    <> "SELECT 1"
    <> T.concat [") AS s" <> T.pack (show i) | i <- [1 .. n]]
