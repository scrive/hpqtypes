module Database.PostgreSQL.PQTypes.Cursor
  ( CursorName(..)
  , CursorSettings
  , scroll
  , withHold
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
import Data.Typeable
import qualified Data.Semigroup as SG

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.SQL
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.Utils

-- | Name of a cursor.
newtype CursorName sql = CursorName { unCursorName :: sql }
  deriving (Eq, Ord, Typeable)

instance IsString sql => IsString (CursorName sql) where
  fromString = CursorName . fromString

instance Show sql => Show (CursorName sql) where
  showsPrec n (CursorName name) = ("Cursor " ++) . showsPrec n name

----------------------------------------

-- | Settings for a cursor. To adjust the settings use corresponding Monoid
-- instance. Default (empty) settings define the cursor as @NO SCROLL@ and
-- @WITHOUT HOLD@ and they can be adjusted using respectively 'scroll' and
-- 'withHold'.
data CursorSettings = CursorSettings
  { csScroll   :: !Bool
  , csWithHold :: !Bool
  }

instance SG.Semigroup CursorSettings where
  a <> b = CursorSettings { csScroll   = csScroll a || csScroll b
                          , csWithHold = csWithHold a || csWithHold b
                          }

instance Monoid CursorSettings where
  mempty = CursorSettings False False

-- | Declare cursor as @SCROLL@. Cursors declared as such can be scrolled in all
-- directions, otherwise only forward.
scroll :: CursorSettings
scroll = CursorSettings True False

-- | Declare cursor as @WITH HOLD@. Cursors NOT declared as such can only be
-- declared within a transaction block and they're automatically closed once the
-- transaction finishes, otherwise they're independent of the current
-- transaction and can be declared even if no transaction is active.
withHold :: CursorSettings
withHold = CursorSettings False True

----------------------------------------

-- | Data representing a created cursor.
data Cursor sql = Cursor !(CursorName sql) !sql
  deriving (Eq, Ord, Show, Typeable)

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
  deriving (Eq, Ord, Show, Typeable)

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
  :: (IsString sql, IsSQL sql, Monoid sql, MonadDB m, MonadMask m)
  => CursorName sql
  -> CursorSettings
  -> sql
  -> (Cursor sql -> m r)
  -> m r
withCursor name CursorSettings{..} sql k = bracket_
  (runQuery_ declareCursor)
  (runQuery_ $ "CLOSE" <+> unCursorName name)
  (k $ Cursor name sql)
  where
    declareCursor = smconcat
      [ "DECLARE"
      , unCursorName name
      , if csScroll then "SCROLL" else "NO SCROLL"
      , "CURSOR"
      , if csWithHold then "WITH HOLD" else "WITHOUT HOLD"
      , "FOR"
      , sql
      ]

-- | Version of 'withCursor' without the @sql@ type parameter for convenience.
withCursorSQL
  :: (MonadDB m, MonadMask m)
  => CursorName SQL
  -> CursorSettings
  -> SQL
  -> (Cursor SQL -> m r)
  -> m r
withCursorSQL = withCursor

-- | Retrieve rows from a query using a cursor. See
-- https://www.postgresql.org/docs/current/sql-fetch.html for more information.
cursorFetch
  :: (IsSQL sql, IsString sql, Monoid sql, MonadDB m)
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
  :: (IsSQL sql, IsString sql, Monoid sql, MonadDB m)
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
