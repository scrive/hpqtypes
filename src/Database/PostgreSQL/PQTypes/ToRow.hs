module Database.PostgreSQL.PQTypes.ToRow
  ( (:++:) (..)
  , ToRow (..)
  ) where

import Data.Functor.Identity

import Database.PostgreSQL.PQTypes.ToSQL

-- | Concatenation of rows, e.g. for passing more query parameters than the
-- largest tuple with a 'ToRow' instance can hold.
data a :++: b = a :++: b
  deriving stock (Eq, Ord, Show)

-- | Class which represents \"from Haskell tuple to list
-- of query parameters\" transformation.
class ToRow row where
  -- | Convert a tuple to a list of query parameters.
  toRow :: row -> [PQParam]

instance (ToRow row1, ToRow row2) => ToRow (row1 :++: row2) where
  toRow (row1 :++: row2) = toRow row1 ++ toRow row2

instance ToRow () where
  toRow () = []

instance ToSQL a => ToRow (Identity a) where
  toRow (Identity a) = [toPQParam a]

instance
  ( ToSQL a1
  , ToSQL a2
  )
  => ToRow (a1, a2)
  where
  toRow (a1, a2) =
    [toPQParam a1, toPQParam a2]

instance
  ( ToSQL a1
  , ToSQL a2
  , ToSQL a3
  )
  => ToRow (a1, a2, a3)
  where
  toRow (a1, a2, a3) =
    [toPQParam a1, toPQParam a2, toPQParam a3]

instance
  ( ToSQL a1
  , ToSQL a2
  , ToSQL a3
  , ToSQL a4
  )
  => ToRow (a1, a2, a3, a4)
  where
  toRow (a1, a2, a3, a4) =
    [toPQParam a1, toPQParam a2, toPQParam a3, toPQParam a4]

instance
  ( ToSQL a1
  , ToSQL a2
  , ToSQL a3
  , ToSQL a4
  , ToSQL a5
  )
  => ToRow (a1, a2, a3, a4, a5)
  where
  toRow (a1, a2, a3, a4, a5) =
    [toPQParam a1, toPQParam a2, toPQParam a3, toPQParam a4, toPQParam a5]

instance
  ( ToSQL a1
  , ToSQL a2
  , ToSQL a3
  , ToSQL a4
  , ToSQL a5
  , ToSQL a6
  )
  => ToRow (a1, a2, a3, a4, a5, a6)
  where
  toRow (a1, a2, a3, a4, a5, a6) =
    [ toPQParam a1
    , toPQParam a2
    , toPQParam a3
    , toPQParam a4
    , toPQParam a5
    , toPQParam a6
    ]

instance
  ( ToSQL a1
  , ToSQL a2
  , ToSQL a3
  , ToSQL a4
  , ToSQL a5
  , ToSQL a6
  , ToSQL a7
  )
  => ToRow (a1, a2, a3, a4, a5, a6, a7)
  where
  toRow (a1, a2, a3, a4, a5, a6, a7) =
    [ toPQParam a1
    , toPQParam a2
    , toPQParam a3
    , toPQParam a4
    , toPQParam a5
    , toPQParam a6
    , toPQParam a7
    ]

instance
  ( ToSQL a1
  , ToSQL a2
  , ToSQL a3
  , ToSQL a4
  , ToSQL a5
  , ToSQL a6
  , ToSQL a7
  , ToSQL a8
  )
  => ToRow (a1, a2, a3, a4, a5, a6, a7, a8)
  where
  toRow (a1, a2, a3, a4, a5, a6, a7, a8) =
    [ toPQParam a1
    , toPQParam a2
    , toPQParam a3
    , toPQParam a4
    , toPQParam a5
    , toPQParam a6
    , toPQParam a7
    , toPQParam a8
    ]

instance
  ( ToSQL a1
  , ToSQL a2
  , ToSQL a3
  , ToSQL a4
  , ToSQL a5
  , ToSQL a6
  , ToSQL a7
  , ToSQL a8
  , ToSQL a9
  )
  => ToRow (a1, a2, a3, a4, a5, a6, a7, a8, a9)
  where
  toRow (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
    [ toPQParam a1
    , toPQParam a2
    , toPQParam a3
    , toPQParam a4
    , toPQParam a5
    , toPQParam a6
    , toPQParam a7
    , toPQParam a8
    , toPQParam a9
    ]

instance
  ( ToSQL a1
  , ToSQL a2
  , ToSQL a3
  , ToSQL a4
  , ToSQL a5
  , ToSQL a6
  , ToSQL a7
  , ToSQL a8
  , ToSQL a9
  , ToSQL a10
  )
  => ToRow (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  where
  toRow (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =
    [ toPQParam a1
    , toPQParam a2
    , toPQParam a3
    , toPQParam a4
    , toPQParam a5
    , toPQParam a6
    , toPQParam a7
    , toPQParam a8
    , toPQParam a9
    , toPQParam a10
    ]
