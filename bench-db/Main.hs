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
import GHC.Generics
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

-- | Number of elements of the big scalar array used for benchmarking the
-- array decoder in isolation.
bigArraySize :: Int32
bigArraySize = 100000

----------------------------------------

data Child = Child Int32 T.Text Double UTCTime Integer
  deriving stock (Generic)
  deriving anyclass (NFData)

instance FromSQL Child where
  fromSQL = decodeComposite genericDecoder

data Parent = Parent Int32 T.Text Double UTCTime Integer [Child]
  deriving stock (Generic)
  deriving anyclass (NFData)

parentDecoder :: RowDecoder [Child] -> RowDecoder Parent
parentDecoder children =
  Parent
    <$> fromSQL
    <*> fromSQL
    <*> fromSQL
    <*> fromSQL
    <*> fromSQL
    <*> children

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

dropTables :: DBT IO ()
dropTables = do
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
  parents <- fetchMany . parentDecoder $ pure []
  -- Make sure that decoders fully ran.
  liftBase . evaluate $ rnf parents
  pure $ length parents

selectData :: DBT IO (Int, Int)
selectData = do
  runSQL_ $
    mconcat
      [ "SELECT p.id, p.t, p.d, p.ts, p.n"
      , ", ARRAY(SELECT (c.id, c.t, c.d, c.ts, c.n)"
      , "        FROM bench_children_ c WHERE c.parent_id = p.id ORDER BY c.id)"
      , " FROM bench_parents_ p ORDER BY p.id"
      ]
  parents <- fetchMany $ parentDecoder fromSQL
  -- Make sure that decoders fully ran.
  liftBase . evaluate $ rnf parents
  pure (length parents, sum $ map (\(Parent _ _ _ _ _ cs) -> length cs) parents)

selectBigArray :: DBT IO Int
selectBigArray = do
  runQuery_ $
    rawSQL "SELECT ARRAY(SELECT generate_series(1, $1))::int4[]" (Identity bigArraySize)
  xs <- fetchOne $ fromSQL @[Int32]
  -- Make sure that decoders fully ran.
  liftBase . evaluate $ rnf xs
  pure $ length xs

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
  let ConnectionSource cs = simpleSource defaultConnectionSettings {csConnInfo = connString}
      runDB :: DBT IO a -> IO a
      runDB = runDBT cs defaultTransactionSettings
  base <- getCurrentTime
  runDB createTables
  (`finally` runDB dropTables) $ do
    (insertTime, ()) <- timed . runDB $ insertData base
    (selectParentsTime, nParentsOnly) <- timed $ runDB selectParents
    (selectTime, (nParents, nChildren)) <- timed $ runDB selectData
    (bigArrayTime, nElems) <- timed $ runDB selectBigArray
    printf "Insertion: %.3f s (%d parents + %d children)\n" insertTime numRecords numRecords
    printf "Selection (parents only): %.3f s (%d parents decoded)\n" selectParentsTime nParentsOnly
    printf "Selection (with children): %.3f s (%d parents with %d children decoded)\n" selectTime nParents nChildren
    printf "Selection (big array): %.3f s (%d elements decoded)\n" bigArrayTime nElems
