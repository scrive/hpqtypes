module Main (main) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Base
import Data.Int
import Data.Text qualified as T
import Data.Time
import Database.PostgreSQL.PQTypes
import GHC.Clock
import System.Environment
import System.Exit
import Text.Printf

-- | Number of records inserted into each of the two tables.
numRecords :: Int
numRecords = 50000

-- | Number of children per parent. Only the first @numRecords \`div\`
-- childrenPerParent@ parents get children, so the selection phase decodes
-- both non-trivial and empty arrays.
childrenPerParent :: Int
childrenPerParent = 10

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
  runSQL_ "DROP TYPE bench_child_"
  runSQL_ "DROP TABLE bench_children_"
  runSQL_ "DROP TABLE bench_parents_"

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

selectParents :: DBT IO Int
selectParents = do
  runSQL_ "SELECT p.id, p.t, p.d, p.ts, p.n FROM bench_parents_ p ORDER BY p.id"
  parents <- fetchMany $ \(pid, t, d, ts, n) -> Parent pid t d ts n []
  -- Make sure that decoders fully ran.
  liftBase . evaluate $ rnf parents
  pure $ length parents

selectData :: DBT IO (Int, Int)
selectData = do
  runSQL_ $
    mconcat
      [ "SELECT p.id, p.t, p.d, p.ts, p.n"
      , ", ARRAY(SELECT (c.id, c.t, c.d, c.ts, c.n)::bench_child_"
      , "        FROM bench_children_ c WHERE c.parent_id = p.id ORDER BY c.id)"
      , " FROM bench_parents_ p ORDER BY p.id"
      ]
  parents <- fetchMany $ \(pid, t, d, ts, n, CompositeArray1 children) ->
    Parent pid t d ts n children
  -- Make sure that decoders fully ran.
  liftBase . evaluate $ rnf parents
  pure (length parents, sum $ map (\(Parent _ _ _ _ _ cs) -> length cs) parents)

----------------------------------------

timed :: IO a -> IO (Double, a)
timed action = do
  t1 <- getMonotonicTime
  a <- action
  t2 <- getMonotonicTime
  pure (t2 - t1, a)

main :: IO ()
main = do
  connString <-
    getArgs >>= \case
      [connString] -> pure $ T.pack connString
      _ -> do
        prog <- getProgName
        putStrLn $ "Usage: " <> prog <> " <connection info string>"
        exitFailure
  let settings = defaultConnectionSettings {csConnInfo = connString}
      ConnectionSource cs = simpleSource settings
      -- Registration of composites happens at connection time, so
      -- bench_child_ needs to exist before this source is used.
      ConnectionSource csComposite =
        simpleSource settings {csComposites = ["bench_child_"]}
      runDB :: ConnectionSourceM IO -> DBT IO a -> IO a
      runDB c = runDBT c defaultTransactionSettings
  base <- getCurrentTime
  runDB cs createTables
  (`finally` runDB cs dropTables) $ do
    (insertTime, ()) <- timed . runDB cs $ insertData base
    (selectParentsTime, nParentsOnly) <- timed $ runDB cs selectParents
    (selectTime, (nParents, nChildren)) <- timed $ runDB csComposite selectData
    printf "Insertion: %.3f s (%d parents + %d children)\n" insertTime numRecords numRecords
    printf "Selection (parents only): %.3f s (%d parents decoded)\n" selectParentsTime nParentsOnly
    printf "Selection (with children): %.3f s (%d parents with %d children decoded)\n" selectTime nParents nChildren
