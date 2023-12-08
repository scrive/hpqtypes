module Database.PostgreSQL.PQTypes.Cursor
  ( CursorName(..)
  , Scroll(..)
  , Hold(..)
  , Cursor
  , CursorDirection(..)
  , cursorName
  , cursorQuery
  , withCursor
  , withCursorSQL
  , cursorFetch
  , cursorFetch_
  , cursorMove
  , cursorMove_
  ) where

import Control.Monad
import Control.Monad.Catch
import Data.String
import GHC.Stack

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.SQL
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.Utils

-- | Name of a cursor.
newtype CursorName sql = CursorName { unCursorName :: sql }
  deriving (Eq, Ord)

instance IsString sql => IsString (CursorName sql) where
  fromString = CursorName . fromString

instance Show sql => Show (CursorName sql) where
  showsPrec n (CursorName name) = ("Cursor " ++) . showsPrec n name

----------------------------------------

-- | Defines whether a cursor will be declared as @SCROLL@ or @NO
-- SCROLL@. Scrollable cursors can be scrolled in all directions, otherwise only
-- forward.
data Scroll = Scroll | NoScroll
  deriving (Eq, Ord, Show)

-- | Defines whether a cursor will be declared as @WITH HOLD@ or @WITHOUT HOLD@.
--
-- From the PostgreSQL manual: WITH HOLD specifies that the cursor can continue
-- to be used after the transaction that created it successfully commits.
-- WITHOUT HOLD specifies that the cursor cannot be used outside of the
-- transaction that created it.
data Hold = Hold | NoHold
  deriving (Eq, Ord, Show)

-- | Data representing a created cursor.
data Cursor sql = Cursor !(CursorName sql) !sql
  deriving (Eq, Ord, Show)

----------------------------------------

-- | Direction in which to move the cursor. Note that cursors declared as @NO
-- SCROLL@ can only move forward (i.e. only 'CD_Next', 'CD_Forward_All' and
-- 'CD_Forward' is allowed).
data CursorDirection
  = CD_Next
  | CD_Prior
  | CD_First
  | CD_Last
  | CD_Forward_All
  | CD_Backward_All
  | CD_Absolute Int
  | CD_Relative Int
  | CD_Forward  Int
  | CD_Backward Int
  deriving (Eq, Ord, Show)

cursorDirectionToSQL :: (IsString sql, IsSQL sql, Monoid sql) => CursorDirection -> sql
cursorDirectionToSQL = \case
  CD_Next         -> "NEXT"
  CD_Prior        -> "PRIOR"
  CD_First        -> "FIRST"
  CD_Last         -> "LAST"
  CD_Forward_All  -> "FORWARD ALL"
  CD_Backward_All -> "BACKWARD ALL"
  CD_Absolute n   -> "ABSOLUTE" <+> unsafeSQL (show n)
  CD_Relative n   -> "RELATIVE" <+> unsafeSQL (show n)
  CD_Forward n    -> "FORWARD"  <+> unsafeSQL (show n)
  CD_Backward n   -> "BACKWARD" <+> unsafeSQL (show n)

----------------------------------------

-- | Retrieve the name of a cursor.
cursorName :: Cursor sql -> CursorName sql
cursorName (Cursor name _) = name

-- | Retrieve SQL query used to create a cursor.
cursorQuery :: Cursor sql -> sql
cursorQuery (Cursor _ query) = query

-- | Create a cursor from the SQL query and use it within the given context.
withCursor
  :: (HasCallStack, IsString sql, IsSQL sql, Monoid sql, MonadDB m, MonadMask m)
  => CursorName sql
  -> Scroll
  -> Hold
  -> sql
  -> (Cursor sql -> m r)
  -> m r
withCursor name scroll hold sql k = bracket_
  (runQuery_ declareCursor)
  (runQuery_ closeCursor)
  (k $ Cursor name sql)
  where
    declareCursor = smconcat
      [ "DECLARE"
      , unCursorName name
      , case scroll of
          Scroll   -> "SCROLL"
          NoScroll -> "NO SCROLL"
      , "CURSOR"
      , case hold of
          Hold   -> "WITH HOLD"
          NoHold -> "WITHOUT HOLD"
      , "FOR"
      , sql
      ]

    -- Because the cursor might potentially be closed within the continuation
    -- (either by an explicit CLOSE or finishing the current transaction), we
    -- need to supress a potential 'InvalidCursorName' exception.
    closeCursor = smconcat
      [ "DO $$"
      , "BEGIN"
      , "  EXECUTE 'CLOSE" <+> unCursorName name <+> "';"
      , "EXCEPTION WHEN invalid_cursor_name THEN"
      , "END $$"
      ]

-- | Version of 'withCursor' without the @sql@ type parameter for convenience.
withCursorSQL
  :: (MonadDB m, MonadMask m)
  => CursorName SQL
  -> Scroll
  -> Hold
  -> SQL
  -> (Cursor SQL -> m r)
  -> m r
withCursorSQL = withCursor

-- | Retrieve rows from a query using a cursor. See
-- https://www.postgresql.org/docs/current/sql-fetch.html for more information.
cursorFetch
  :: (HasCallStack, IsSQL sql, IsString sql, Monoid sql, MonadDB m)
  => Cursor sql
  -> CursorDirection
  -> m Int
cursorFetch cursor direction = runQuery $ smconcat
  [ "FETCH"
  , cursorDirectionToSQL direction
  , "FROM"
  , unCursorName $ cursorName cursor
  ]

-- | Same as 'cursorFetch', except the result (i.e. the number of fetched rows)
-- is ignored.
cursorFetch_ :: (IsSQL sql, IsString sql, Monoid sql, MonadDB m)
  => Cursor sql
  -> CursorDirection
  -> m ()
cursorFetch_ cursor = void . cursorFetch cursor

-- | Move a cursor to a specific position. It works exactly like 'cursorFetch',
-- except it only positions the cursor and does not return rows. See
-- https://www.postgresql.org/docs/current/sql-move.html for more information.
cursorMove
  :: (HasCallStack, IsSQL sql, IsString sql, Monoid sql, MonadDB m)
  => Cursor sql
  -> CursorDirection
  -> m Int
cursorMove cursor direction = runQuery $ smconcat
  [ "MOVE"
  , cursorDirectionToSQL direction
  , "FROM"
  , unCursorName $ cursorName cursor
  ]

-- | Same as 'cursorMove', except the result (i.e. the number of rows that would
-- be fetched) is ignored.
cursorMove_
  :: (IsSQL sql, IsString sql, Monoid sql, MonadDB m)
  => Cursor sql
  -> CursorDirection
  -> m ()
cursorMove_ cursor = void . cursorMove cursor
