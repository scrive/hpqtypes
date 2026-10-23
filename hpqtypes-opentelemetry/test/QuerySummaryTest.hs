module QuerySummaryTest (querySummaryTest) where

import Control.Monad
import Data.Maybe
import Data.Text qualified as T
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Database.PostgreSQL.PQTypes.OpenTelemetry.QuerySummary

querySummaryTest :: Test
querySummaryTest =
  testGroup
    "Query summary is extracted from the query"
    [ querySummarySelectTest
    , querySummaryJoinTest
    , querySummaryLexingTest
    , querySummaryCteTest
    , querySummaryModificationTest
    , querySummaryDdlTest
    , querySummaryStoredProcedureTest
    , querySummaryOtherStatementTest
    ]

-- | Check the operation name, the rendered summary and the collection that are
-- extracted from a query.
summaryTestCase :: T.Text -> Maybe T.Text -> T.Text -> Maybe T.Text -> Test
summaryTestCase query operation rendered collection = testCase (T.unpack query) $ do
  let qs = parseQuerySummary query
  assertEqual "Operation name" operation (qsOperationName qs)
  assertEqual "Summary" rendered (renderQuerySummary qs)
  assertEqual "Collection" collection (querySummaryCollection qs)

querySummarySelectTest :: Test
querySummarySelectTest =
  testGroup
    "SELECT"
    [ summaryTestCase
        "select x from public.users u where u.id = any($1)"
        (Just "select")
        "select public.users"
        (Just "public.users")
    , summaryTestCase
        "Select * From a Join b On true"
        (Just "Select")
        "Select a b"
        Nothing
    , summaryTestCase
        "SELECT * FROM a WHERE x IS DISTINCT FROM y OR x IS NOT DISTINCT FROM z"
        (Just "SELECT")
        "SELECT a"
        (Just "a")
    , summaryTestCase "SELECT * FROM a, b" (Just "SELECT") "SELECT a b" Nothing
    , summaryTestCase "SELECT * FROM t FOR UPDATE" (Just "SELECT") "SELECT t" (Just "t")
    , summaryTestCase "SELECT * FROM generate_series(1, 10) g" (Just "SELECT") "SELECT" Nothing
    , summaryTestCase
        "SELECT * FROM a WHERE e = (SELECT max(v) FROM f)"
        (Just "SELECT")
        "SELECT a SELECT f"
        Nothing
    , summaryTestCase "(SELECT 1 FROM a) UNION (SELECT 2 FROM b)" (Just "SELECT") "SELECT a SELECT b" Nothing
    , summaryTestCase "VALUES (1), (2)" (Just "VALUES") "VALUES" Nothing
    , summaryTestCase "SELECT 1" (Just "SELECT") "SELECT" Nothing
    ]

querySummaryJoinTest :: Test
querySummaryJoinTest =
  testGroup
    "JOIN"
    [ summaryTestCase
        "SELECT * FROM a INNER JOIN b ON a.id = b.a_id"
        (Just "SELECT")
        "SELECT a b"
        Nothing
    , summaryTestCase
        "SELECT * FROM a LEFT OUTER JOIN b ON true RIGHT JOIN c ON true FULL OUTER JOIN d ON true"
        (Just "SELECT")
        "SELECT a b c d"
        Nothing
    , summaryTestCase
        "SELECT * FROM a CROSS JOIN b NATURAL JOIN c"
        (Just "SELECT")
        "SELECT a b c"
        Nothing
    , summaryTestCase
        "SELECT * FROM a JOIN b USING (id, name)"
        (Just "SELECT")
        "SELECT a b"
        Nothing
    , summaryTestCase
        "SELECT * FROM public.a AS x JOIN s.b y ON x.id = y.id"
        (Just "SELECT")
        "SELECT public.a s.b"
        Nothing
    , summaryTestCase
        "SELECT * FROM a x JOIN a y ON x.parent_id = y.id"
        (Just "SELECT")
        "SELECT a"
        (Just "a")
    , summaryTestCase
        "SELECT * FROM a, b JOIN c ON true"
        (Just "SELECT")
        "SELECT a b c"
        Nothing
    , summaryTestCase
        "SELECT * FROM (a JOIN b ON true) JOIN c ON true"
        (Just "SELECT")
        "SELECT a b c"
        Nothing
    , summaryTestCase
        "SELECT * FROM a JOIN unnest(a.xs) u ON true"
        (Just "SELECT")
        "SELECT a"
        (Just "a")
    , summaryTestCase
        "SELECT * FROM a JOIN (SELECT id FROM b) AS s ON s.id = a.id JOIN c ON true"
        (Just "SELECT")
        "SELECT a c SELECT b"
        Nothing
    , summaryTestCase
        "SELECT * FROM a CROSS JOIN LATERAL (SELECT * FROM b WHERE b.a_id = a.id) l"
        (Just "SELECT")
        "SELECT a SELECT b"
        Nothing
    , summaryTestCase
        "SELECT * FROM a JOIN b ON b.id = (SELECT max(id) FROM c) JOIN d ON true"
        (Just "SELECT")
        "SELECT a b d SELECT c"
        Nothing
    , summaryTestCase
        "SELECT * FROM a JOIN b ON a.x = b.y LEFT JOIN s.c AS cc USING (z), d WHERE e = (SELECT max(v) FROM f)"
        (Just "SELECT")
        "SELECT a b s.c d SELECT f"
        Nothing
    , summaryTestCase
        "UPDATE a SET x = b.x FROM b JOIN c ON b.id = c.id WHERE a.id = b.id"
        (Just "UPDATE")
        "UPDATE a b c"
        Nothing
    , summaryTestCase
        "DELETE FROM a USING b JOIN c ON b.id = c.id WHERE a.id = b.id"
        (Just "DELETE")
        "DELETE a b c"
        Nothing
    ]

querySummaryLexingTest :: Test
querySummaryLexingTest =
  testGroup
    "Comments, literals and quoted names"
    [ summaryTestCase
        "SELECT extract(YEAR FROM ts), substring(x FROM 2) FROM t -- FROM a\n /* FROM /* b */ c */"
        (Just "SELECT")
        "SELECT t"
        (Just "t")
    , summaryTestCase
        "SELECT 'it''s FROM a', E'\\' FROM b', $tag$ FROM c $tag$, $1 FROM \"Quoted \"\"Name\"\"\""
        (Just "SELECT")
        "SELECT \"Quoted \"\"Name\"\"\""
        (Just "\"Quoted \"\"Name\"\"\"")
    , summaryTestCase "" Nothing "" Nothing
    ]

querySummaryCteTest :: Test
querySummaryCteTest =
  testGroup
    "WITH"
    [ summaryTestCase
        "WITH RECURSIVE x(a) AS (SELECT 1 FROM base), y AS (DELETE FROM old RETURNING *) INSERT INTO z SELECT * FROM x, y"
        (Just "INSERT")
        "SELECT base DELETE old INSERT z SELECT"
        Nothing
    , summaryTestCase
        "WITH x AS (SELECT * FROM a) SELECT * FROM x JOIN b ON true"
        (Just "SELECT")
        "SELECT a SELECT b"
        Nothing
    ]

querySummaryModificationTest :: Test
querySummaryModificationTest =
  testGroup
    "INSERT, UPDATE, DELETE and MERGE"
    [ summaryTestCase
        "INSERT INTO high_value_customers (id, name) SELECT id, name FROM customers WHERE total_spend > $1"
        (Just "INSERT")
        "INSERT high_value_customers SELECT customers"
        Nothing
    , summaryTestCase
        "INSERT INTO t (a) VALUES ($1) ON CONFLICT (a) DO UPDATE SET a = EXCLUDED.a RETURNING a"
        (Just "INSERT")
        "INSERT t"
        (Just "t")
    , summaryTestCase
        "UPDATE ONLY t AS q SET a = $1 FROM u WHERE q.id = u.id"
        (Just "UPDATE")
        "UPDATE t u"
        Nothing
    , summaryTestCase "DELETE FROM t USING u WHERE t.id = u.id" (Just "DELETE") "DELETE t u" Nothing
    , summaryTestCase
        "MERGE INTO t USING s ON t.id = s.id WHEN MATCHED THEN UPDATE SET x = 1 WHEN NOT MATCHED THEN INSERT (id) VALUES (s.id)"
        (Just "MERGE")
        "MERGE t s"
        Nothing
    ]

querySummaryDdlTest :: Test
querySummaryDdlTest =
  testGroup
    "DDL"
    [ summaryTestCase "CREATE TABLE IF NOT EXISTS foo (id int)" (Just "CREATE TABLE") "CREATE TABLE foo" (Just "foo")
    , summaryTestCase "CREATE UNIQUE INDEX CONCURRENTLY ON foo (id)" (Just "CREATE INDEX") "CREATE INDEX foo" (Just "foo")
    , summaryTestCase
        "CREATE OR REPLACE VIEW v AS SELECT * FROM a JOIN b ON true"
        (Just "CREATE VIEW")
        "CREATE VIEW v SELECT a b"
        Nothing
    , summaryTestCase "DROP TABLE IF EXISTS a, b CASCADE" (Just "DROP TABLE") "DROP TABLE a b" Nothing
    , summaryTestCase "TRUNCATE TABLE a" (Just "TRUNCATE") "TRUNCATE a" (Just "a")
    ]

querySummaryStoredProcedureTest :: Test
querySummaryStoredProcedureTest =
  testGroup
    "Stored procedures"
    [ procedureTestCase
        "CALL some_stored_procedure($1, $2)"
        "CALL some_stored_procedure"
        (Just "some_stored_procedure")
    , procedureTestCase
        "call public.\"MyProc\"()"
        "call public.\"MyProc\""
        (Just "public.\"MyProc\"")
    , -- Functions are not stored procedures.
      procedureTestCase "SELECT some_function($1)" "SELECT" Nothing
    , procedureTestCase "SELECT * FROM a" "SELECT a" Nothing
    ]
  where
    procedureTestCase query rendered procedure = testCase (T.unpack query) $ do
      let qs = parseQuerySummary query
      assertEqual "Summary" rendered (renderQuerySummary qs)
      assertEqual "Stored procedure" procedure (querySummaryStoredProcedure qs)
      -- A stored procedure is not a collection.
      when (isJust procedure) $
        assertEqual "Collection" Nothing (querySummaryCollection qs)

querySummaryOtherStatementTest :: Test
querySummaryOtherStatementTest =
  testGroup
    "Other statements"
    [ summaryTestCase "BEGIN ISOLATION LEVEL SERIALIZABLE" (Just "BEGIN") "BEGIN" Nothing
    , summaryTestCase "COMMIT" (Just "COMMIT") "COMMIT" Nothing
    ]
