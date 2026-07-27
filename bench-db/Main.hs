module Main (main) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Int
import Data.Pool (defaultPoolConfig)
import Data.Text qualified as T
import Data.Time
import Database.PostgreSQL.PQTypes
import System.Environment
import Test.Tasty.Bench

-- | Number of records inserted into each of the two tables.
numRecords :: Int
numRecords = 50000

-- | Number of children per parent. Only the first @numRecords \`div\`
-- childrenPerParent@ parents get children, so the selection phase decodes
-- both non-trivial and empty arrays.
childrenPerParent :: Int
childrenPerParent = 10

-- | Number of scalars fetched by 'selectBigArray' and 'selectManyRows'.
-- They deliver the same amount of data, as one array and as that many rows
-- respectively, so that the array decoder and the row decoder can be
-- compared against each other.
bigArraySize :: Int32
bigArraySize = 100000

----------------------------------------

data Child = Child Int32 T.Text Double UTCTime Integer
data Parent = Parent Int32 T.Text Double UTCTime Integer [Child]

instance NFData Child where
  rnf (Child a b c d e) =
    rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

instance NFData Parent where
  rnf (Parent a b c d e f) =
    rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f

type instance CompositeRow Child = (Int32, T.Text, Double, UTCTime, Integer)

instance PQFormat Child where
  pqFormat = "%bench_child_"

instance CompositeFromSQL Child where
  toComposite (cid, t, d, ts, n) = Child cid t d ts n

-- | Deterministic record with the given id.
recordData :: UTCTime -> Int -> (Int32, T.Text, Double, UTCTime, Integer)
recordData base i =
  ( fromIntegral i
  , T.pack $ "record " <> show i <> " with some textual payload"
  , fromIntegral i * 1.5
  , addUTCTime (fromIntegral i) base
  , 2 ^ (70 :: Int) + fromIntegral i
  )

----------------------------------------

createTables :: DBT IO ()
createTables = do
  runSQL_ $
    mconcat
      [ "CREATE TABLE bench_parents_ ("
      , "  id INTEGER NOT NULL"
      , ", t TEXT NOT NULL"
      , ", d DOUBLE PRECISION NOT NULL"
      , ", ts TIMESTAMPTZ NOT NULL"
      , ", n NUMERIC NOT NULL"
      , ", PRIMARY KEY (id)"
      , ")"
      ]
  runSQL_ $
    mconcat
      [ "CREATE TABLE bench_children_ ("
      , "  id INTEGER NOT NULL"
      , ", parent_id INTEGER NOT NULL"
      , ", t TEXT NOT NULL"
      , ", d DOUBLE PRECISION NOT NULL"
      , ", ts TIMESTAMPTZ NOT NULL"
      , ", n NUMERIC NOT NULL"
      , ", PRIMARY KEY (id)"
      , ", FOREIGN KEY (parent_id) REFERENCES bench_parents_ (id)"
      , ")"
      ]
  runSQL_ "CREATE INDEX bench_children_parent_id_idx_ ON bench_children_ (parent_id)"
  runSQL_ $
    mconcat
      [ "CREATE TYPE bench_child_ AS ("
      , "  id INTEGER"
      , ", t TEXT"
      , ", d DOUBLE PRECISION"
      , ", ts TIMESTAMPTZ"
      , ", n NUMERIC"
      , ")"
      ]

dropTables :: DBT IO ()
dropTables = do
  runSQL_ "DROP TYPE IF EXISTS bench_child_"
  runSQL_ "DROP TABLE IF EXISTS bench_children_"
  runSQL_ "DROP TABLE IF EXISTS bench_parents_"

-- | Empty the tables so that 'insertData' can be run again.
truncateTables :: DBT IO ()
truncateTables = runSQL_ "TRUNCATE bench_children_, bench_parents_"

insertData :: UTCTime -> DBT IO ()
insertData base = do
  forM_ [0 .. numRecords - 1] $ \i -> do
    runQuery_ $
      rawSQL
        "INSERT INTO bench_parents_ (id, t, d, ts, n) VALUES ($1, $2, $3, $4, $5)"
        (recordData base i)
  forM_ [0 .. numRecords - 1] $ \i -> do
    let (cid, t, d, ts, n) = recordData base i
        pid = fromIntegral $ i `div` childrenPerParent :: Int32
    runQuery_ $
      rawSQL
        "INSERT INTO bench_children_ (id, parent_id, t, d, ts, n) VALUES ($1, $2, $3, $4, $5, $6)"
        (cid, pid, t, d, ts, n)

selectParents :: DBT IO [Parent]
selectParents = do
  runSQL_ "SELECT p.id, p.t, p.d, p.ts, p.n FROM bench_parents_ p ORDER BY p.id"
  fetchMany $ \(pid, t, d, ts, n) -> Parent pid t d ts n []

selectData :: DBT IO [Parent]
selectData = do
  runSQL_ $
    mconcat
      [ "SELECT p.id, p.t, p.d, p.ts, p.n"
      , ", ARRAY(SELECT (c.id, c.t, c.d, c.ts, c.n)::bench_child_"
      , "        FROM bench_children_ c WHERE c.parent_id = p.id ORDER BY c.id)"
      , " FROM bench_parents_ p ORDER BY p.id"
      ]
  fetchMany $ \(pid, t, d, ts, n, CompositeArray1 children) ->
    Parent pid t d ts n children

selectBigArray :: DBT IO [Int32]
selectBigArray = do
  runQuery_ $
    rawSQL "SELECT ARRAY(SELECT generate_series(1, $1))::int4[]" (Identity bigArraySize)
  fetchOne $ \(Identity (Array1 elems)) -> elems

-- | The counterpart of 'selectBigArray': the same scalars, delivered as one
-- column of that many rows instead of as one array.
selectManyRows :: DBT IO [Int32]
selectManyRows = do
  runQuery_ $ rawSQL "SELECT generate_series(1, $1)" (Identity bigArraySize)
  fetchMany runIdentity

----------------------------------------

-- | The connection info string is taken from the @CONNINFO@ environment
-- variable, as the command line belongs to @tasty-bench@. If it's not set,
-- the choice is left to @libpq@, i.e. to the @PG*@ variables.
main :: IO ()
main = do
  connInfo <- maybe T.empty T.pack <$> lookupEnv "CONNINFO"
  let settings = defaultConnectionSettings {csConnInfo = connInfo}
  ConnectionSource cs <- pooled settings
  let runDB :: DBT IO a -> IO a
      runDB = runDBT cs defaultTransactionSettings
  base <- getCurrentTime
  runDB $ do
    -- Keep the NOTICEs of the DROPs below out of the benchmark report.
    runSQL_ "SET client_min_messages TO WARNING"
    dropTables
    createTables
    insertData base
  -- Registration of composites happens at connection time, so bench_child_
  -- needs to exist before this source is used.
  ConnectionSource csComposite <- pooled settings {csComposites = ["bench_child_"]}
  let runDBComposite :: DBT IO a -> IO a
      runDBComposite = runDBT csComposite defaultTransactionSettings
  (`finally` runDB dropTables) . defaultMain $
    [ -- Insertion refills the tables it empties, so the selection benchmarks
      -- below see the same data regardless of whether this one ran.
      bench "insert" . nfIO . runDB $ truncateTables >> insertData base
    , bench "select parents" . nfIO $ runDB selectParents
    , bench "select parents with children" . nfIO $ runDBComposite selectData
    , bench "select big array" . nfIO $ runDB selectBigArray
    , bench "select many rows" . nfIO $ runDB selectManyRows
    ]
  where
    -- A pool holding a single connection is used rather than 'simpleSource'
    -- so that establishing one isn't measured by every iteration of a
    -- benchmark.
    pooled settings = poolSource settings $ \connect disconnect ->
      defaultPoolConfig connect disconnect 60 1
