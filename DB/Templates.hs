module DB.Templates
       where

import Text.StringTemplate (StringTemplate, newSTMP)

moduleHeader :: StringTemplate String
moduleHeader = newSTMP $
  unlines
    [ "module $prefix$Models.$tableType$"
    , "  ($tableType$,"
    , "   $accessors;separator=', '$)"
    , "  where"
    , ""
    ]

imports :: StringTemplate String
imports = newSTMP $
  unlines
    ["$imports;separator='\n'$"
    , ""
    ]

dataType :: StringTemplate String
dataType = newSTMP $
  unlines
    [ "data $tableType$ = $tableType$"
    , "  { $fields;separator='\n  , '$"
    , "  } deriving Show"
    , ""
    ]

dbModelInstance :: StringTemplate String
dbModelInstance = newSTMP $
  unlines
    [ "instance DBModel $tableType$ where"
    , "  tableName = const \"$tableName$\""
    , "  rowCardinality = const $cardinality$"
    , "  empty     = $tableType$ $undefined;separator=' '$"
    , ""
    , "  fromSqlValues [$fieldNames;separator=', '$] ="
    , "    $tableType$ $fromSqls;separator=' '$"
    , "  fromSqlValues _ ="
    , "    error \"$tableName$.fromSqlValues: assertion failed\""
    , ""
    , "  toSqlValues t ="
    , "    [ $toSqls;separator=', '$ ]"
    , ""
    ]

accessor :: StringTemplate String
accessor = newSTMP $
  unlines
    [ "$accessor$ :: Accessor $table$ $type$"
    , "$accessor$ = Accessor (++ \"$accessor$\") getter setter"
    , "  where getter t   = _$accessor$ t"
    , "        setter t v = t { _$accessor$ = v }"
    , ""
    ]
