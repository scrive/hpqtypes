module Catalog (catalog) where

import Control.Arrow (second)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Function
import Data.Int
import Data.Monoid.Utils
import Data.Pool
import Data.Text qualified as T
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Internal.Utils (mread)
import GHC.Generics
import System.Console.Haskeline

-- | Generic 'putStrLn'.
printLn :: MonadIO m => String -> m ()
printLn = liftIO . putStrLn

----------------------------------------

-- | Representation of a book.
data Book = Book
  { bookID :: Int64
  , bookName :: String
  , bookYear :: Int32
  }
  deriving stock (Generic, Read, Show)

instance FromSQL Book where
  fromSQL = decodeComposite genericDecoder

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
    -- Drop previously created database structures.
    dropStructure = runDBT cs defaultTransactionSettings $ do
      printLn "Dropping tables..."
      runSQL_ "DROP TABLE books_"
      runSQL_ "DROP TABLE authors_"

----------------------------------------

processCommand :: ConnectionSourceM IO -> String -> IO ()
processCommand cs cmd = case parse cmd of
  -- Display authors.
  ("authors", "") -> runDBT cs defaultTransactionSettings $ do
    runSQL_ "SELECT * FROM authors_ ORDER BY name"
    mapDB_ ((,) <$> fromSQL @Int64 <*> fromSQL @String) $ \(aid, name) -> do
      printLn $ show aid <> ":" <+> name
  -- Display books.
  ("books", "") -> runDBT cs defaultTransactionSettings $ do
    runSQL_ "SELECT a.name, ARRAY(SELECT (b.id, b.name, b.year) FROM books_ b WHERE b.author_id = a.id) FROM authors_ a ORDER BY a.name"
    mapDB_ ((,) <$> fromSQL @String <*> fromSQL @[Book]) $ \(author, books) -> do
      printLn $ author <> ":"
      forM_ books $ \book -> printLn $ "*" <+> show book
  -- Insert an author.
  ("insert_author", mname) -> case mread mname of
    Just (name :: String) ->
      runDBT cs defaultTransactionSettings . runQuery_ $
        "INSERT INTO authors_ (name) VALUES (" <?> name <+> ")"
    Nothing -> printLn "Invalid name"
  -- Insert a book.
  ("insert_book", mbook) -> case mread @(String, Int32, Int64) mbook of
    Just record ->
      runDBT cs defaultTransactionSettings . runQuery_ $
        rawSQL "INSERT INTO books_ (name, year, author_id) VALUES ($1, $2, $3)" record
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
catalog :: T.Text -> IO ()
catalog connInfo = do
  let cs = defaultConnectionSettings {csConnInfo = connInfo}
  withCatalog cs $ do
    ConnectionSource pool <- poolSource cs $ \connect disconnect ->
      defaultPoolConfig connect disconnect 1 10
    runInputT defaultSettings . fix $ \next ->
      getInputLine "> "
        >>= maybe
          (printLn "")
          ( \cmd -> do
              when (cmd /= "quit") $ do
                liftIO $ processCommand pool cmd
                next
          )
