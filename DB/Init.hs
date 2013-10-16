module DB.Init (checkDB)
       where

import Control.Monad (unless, mapM_)
import Data.List (sort)
import Database.HDBC (IConnection, getTables, run, withTransaction)

import DB.Schema (tables, tableNames)
import DB.CodeGen

checkDB :: (IConnection conn) => conn -> IO Bool
checkDB connection =
  withTransaction connection $ const $
    do initialized <- isDBInitialized connection
       if initialized
         then return False
         else initializeDB connection >> return True

isDBInitialized :: (IConnection conn) => conn -> IO Bool
isDBInitialized connection =
  do dbTables <- getTables connection
     return $ sort dbTables == tableNames

initializeDB :: (IConnection conn) => conn -> IO ()
initializeDB connection =
  do dbTables <- getTables connection
     mapM_ dropTable dbTables
     mapM_ createTable tables

  where dropTable :: String -> IO ()
        dropTable table =
          run connection ("DROP TABLE " ++ table) [] >> return ()

        createTable :: DBTable -> IO ()
        createTable table =
          run connection (toSql table) [] >> return ()