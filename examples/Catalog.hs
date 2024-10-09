module Catalog (catalog) where

import Control.Arrow (second)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class (liftIO)
import Data.Int
import Data.Monoid.Utils
import Data.Pool
import Data.Text qualified as T
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Internal.Utils (mread)
import System.Console.Haskeline
import System.Environment

-- | Generic 'putStrLn'.
printLn :: MonadBase IO m => String -> m ()
printLn = liftBase . putStrLn

-- | Get connection string from command line argument.
getConnSettings :: IO ConnectionSettings
getConnSettings = do
  args <- getArgs
  case args of
    [conninfo] -> pure defaultConnectionSettings {csConnInfo = T.pack conninfo}
    _ -> do
      prog <- getProgName
      error $ "Usage:" <+> prog <+> "<connection info>"

----------------------------------------

-- | Representation of a book.
data Book = Book
  { bookID :: Int64
  , bookName :: String
  , bookYear :: Int32
  }
  deriving (Read, Show)

-- | Intermediate representation of 'Book'.
type instance CompositeRow Book = (Int64, String, Int32)

instance PQFormat Book where
  pqFormat = "%book_"

instance CompositeFromSQL Book where
  toComposite (bid, name, year) =
    Book
      { bookID = bid
      , bookName = name
      , bookYear = year
      }

withCatalog :: ConnectionSettings -> IO () -> IO ()
withCatalog settings = bracket_ createStructure dropStructure
  where
    ConnectionSource cs = simpleSource settings

    -- Create needed tables and types.
    createStructure = runDBT cs defaultTransactionSettings $ do
      printLn "Creating tables..."
      runSQL_ $
        mconcat
          [ "CREATE TABLE authors_ ("
          , "  id BIGSERIAL NOT NULL"
          , ", name TEXT NOT NULL"
          , ", PRIMARY KEY (id)"
          , ")"
          ]
      runSQL_ $
        mconcat
          [ "CREATE TABLE books_ ("
          , "  id BIGSERIAL NOT NULL"
          , ", name TEXT NOT NULL"
          , ", year INTEGER NOT NULL"
          , ", author_id BIGINT NOT NULL"
          , ", PRIMARY KEY (id)"
          , ", FOREIGN KEY (author_id) REFERENCES authors_ (id)"
          , ")"
          ]
      runSQL_ $
        mconcat
          [ "CREATE TYPE book_ AS ("
          , "  id BIGINT"
          , ", name TEXT"
          , ", year INTEGER"
          , ")"
          ]
    -- Drop previously created database structures.
    dropStructure = runDBT cs defaultTransactionSettings $ do
      printLn "Dropping tables..."
      runSQL_ "DROP TYPE book_"
      runSQL_ "DROP TABLE books_"
      runSQL_ "DROP TABLE authors_"

----------------------------------------

processCommand :: ConnectionSourceM IO -> String -> IO ()
processCommand cs cmd = case parse cmd of
  -- Display authors.
  ("authors", "") -> runDBT cs defaultTransactionSettings $ do
    runSQL_ "SELECT * FROM authors_ ORDER BY name"
    mapDB_ $ \(aid :: Int64, name) -> printLn $ show aid <> ":" <+> name
  -- Display books.
  ("books", "") -> runDBT cs defaultTransactionSettings $ do
    runSQL_ "SELECT a.name, ARRAY(SELECT (b.id, b.name, b.year)::book_ FROM books_ b WHERE b.author_id = a.id) FROM authors_ a ORDER BY a.name"
    mapDB_ $ \(author, CompositeArray1 (books :: [Book])) -> do
      printLn $ author <> ":"
      forM_ books $ \book -> printLn $ "*" <+> show book
  -- Insert an author.
  ("insert_author", mname) -> case mread mname of
    Just (name :: String) ->
      runDBT cs defaultTransactionSettings . runQuery_ $
        "INSERT INTO authors_ (name) VALUES (" <?> name <+> ")"
    Nothing -> printLn "Invalid name"
  -- Insert a book.
  ("insert_book", mbook) -> case mread mbook of
    Just record ->
      runDBT cs defaultTransactionSettings . runQuery_ $
        rawSQL
          "INSERT INTO books_ (name, year, author_id) VALUES ($1, $2, $3)"
          (record :: (String, Int32, Int64))
    Nothing -> printLn "Invalid book record"
  -- Handle unknown commands.
  _ -> printLn $ "Unknown command:" <+> cmd
  where
    parse = second (drop 1) . break (== ' ')

-- | Example chain of commands:
--
-- > insert_author "John Doe"
-- > insert_author "Mary Jane"
-- > authors
-- > insert_book ("The Sunset", 2006, 1)
-- > insert_book ("Waterfall", 2011, 2)
-- > insert_book ("The Sunrise", 2013, 1)
-- > books
--
-- If you want to check out exceptions in action,
-- try inserting a book with invalid author id.
catalog :: IO ()
catalog = do
  cs <- getConnSettings
  withCatalog cs $ do
    ConnectionSource pool <- poolSource (cs {csComposites = ["book_"]}) (\connect disconnect -> defaultPoolConfig connect disconnect 1 10)
    runInputT defaultSettings (loop pool)
  where
    loop pool = do
      getInputLine "> "
        >>= maybe
          (outputStrLn "")
          ( \cmd -> do
              when (cmd /= "quit") $ do
                liftIO $ processCommand pool cmd
                loop pool
          )
