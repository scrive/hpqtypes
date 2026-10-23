module Database.PostgreSQL.PQTypes.Versions where

import Database.PostgreSQL.PQTypes.Model

tableVersions :: Table
tableVersions =
  tblTable
    { tblName = "table_versions"
    , tblVersion = 1
    , tblColumns =
        [ tblColumn {colName = "name", colType = TextT, colNullable = False}
        , tblColumn {colName = "version", colType = IntegerT, colNullable = False}
        ]
    , tblPrimaryKey = pkOnColumn "name"
    }
