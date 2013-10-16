module DB.Schema (tableNames, tables)
       where

import Data.List (sort)

import DB.CodeGen

testId = "id" := NotNull (Type DBInt)
test =
  DBTable "test"
          [ F testId
          , F ("text" := Type DBText) ]
          [ PrimaryKey testId ]

showingId = "id" := NotNull (Type DBInt)
showing   =
  DBTable "showing"
          [ F  showingId
          , F ("datetime" := NotNull (Type DBDateTime))
          , F  pid
          , F ("notes"    := (Type DBText)) ]
          [ PrimaryKey showingId
          , ForeignKey pid performance performanceId ]
            where pid = "performance_id" := NotNull (Type DBInt)

performanceId = "id" := NotNull (Type DBInt)
performance   =
  DBTable "performance"
          [ F performanceId
          , F ("title"    := NotNull (Type DBText))
          , F ("director" := NotNull (Type DBText))
          , F ("author"   := Type DBText) ]
          [ PrimaryKey performanceId ]

areaId = "id" := NotNull (Type DBInt)
area   =
  DBTable "area"
          [ F areaId
          , F ("name" := NotNull (Type DBText)) ]
          [ PrimaryKey areaId ]

row =
  DBTable "row"
          [ F id
          , F ("number" := NotNull (Type DBInt))
          , F aid
          , F ("places" := NotNull (Type DBInt)) ]
          [ PrimaryKey id
          , ForeignKey aid area areaId ]
            where id  = "id"      := NotNull (Type DBInt)
                  aid = "area_id" := NotNull (Type DBInt)

ticket =
  DBTable "ticket"
          [ F  id
          , F ("row_num" := NotNull (Type DBInt))
          , F ("place"   := NotNull (Type DBInt))
          , F ("price"   := NotNull (Type DBInt))
          , F sid
          , F aid ]
          [ PrimaryKey id
          , ForeignKey sid showing showingId
          , ForeignKey aid area areaId ]
            where id  = "id"         := NotNull (Type DBInt)
                  aid = "area_id"    := NotNull (Type DBInt)
                  sid = "showing_id" := NotNull (Type DBInt)

tableNames :: [String]
tableNames = sort $ map tableName tables
  where tableName (DBTable name _ _) = name

tables :: [DBTable]
tables =
  [ showing
  , performance
  , area
  , row
  , ticket ]
