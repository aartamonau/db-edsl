{-# LANGUAGE GADTs, EmptyDataDecls, RankNTypes,
             ViewPatterns, FlexibleInstances, UndecidableInstances,
             MultiParamTypeClasses #-}

module DB.CodeGen
       where

import Control.Monad (when, forM)

import Data.List (intersperse, sort, group, groupBy)
import Data.Char (isAlphaNum, toUpper)

import System.Directory
import System.FilePath

import Text.StringTemplate
import Text.XFormat.Show

import qualified DB.Templates as T

data Unconstrained
data NotNullConstraint

data DBInt      = DBInt
data DBDateTime = DBDateTime
data DBText     = DBText

class SimpleType a
instance SimpleType DBInt
instance SimpleType DBDateTime
instance SimpleType DBText

class (ToSql a, ToHaskell a)    => Splicable a
instance (ToSql a, ToHaskell a) => Splicable a

data DBType t c where
  Type       :: (SimpleType t, Splicable t) => t -> DBType t Unconstrained
  NotNull    :: Splicable t =>
                 DBType t Unconstrained -> DBType t NotNullConstraint

data DBField t c where
  (:=) :: Splicable t => String -> DBType t c -> DBField t c

data DBTableField = forall c t. Splicable t => F (DBField t c)

data DBConstraint where
  PrimaryKey :: Splicable t => DBField t NotNullConstraint -> DBConstraint
  ForeignKey :: Splicable t =>
                DBField t c -> DBTable -> DBField t c' -> DBConstraint

instance ToSql DBConstraint where
  toSql _ (PrimaryKey (name := _)) =
    showf ("PRIMARY KEY (" % name % ")")
  toSql _ (ForeignKey (name := _) (DBTable tname _ _) (name' := _)) =
    showf ("FOREIGN KEY (" % name % ") REFERENCES " % tname % "(" % name' % ")")

data DBTable = DBTable String [DBTableField] [DBConstraint]

class HasName a where
  name           :: a -> String
  normalizedName :: a -> String
  normalizedName = name

instance HasName DBTable where
  name (DBTable name _ _) = name

  normalizedName table = normalize (name table)
    where normalize = concatMap capitalize . alphaNumOnly . groupBy groupPred
            where groupPred x y | isAlphaNum x && isAlphaNum y = True
                                | otherwise                    = False
                  alphaNumOnly = filter (isAlphaNum . head)
                  capitalize (x : xs) = toUpper x : xs

instance HasName (DBField t c) where
  name (name := _) = name

instance HasName DBTableField where
  name (F field) = name field

data Backend =
  Backend { dbInt      :: String
          , dbText     :: String
          , dbDateTime :: String }

sqlite3Backend = Backend { dbInt      = "INTEGER"
                         , dbText     = "TEXT"
                         , dbDateTime = "DATE" }

otherBackend   = Backend { dbInt      = "NUMBER"
                         , dbText     = "STRING"
                         , dbDateTime = "DATETIME" }

class ToSql a where
  toSql :: Backend -> a -> String

instance ToSql DBInt where
  toSql backend _ = dbInt backend

instance ToSql DBDateTime where
  toSql backend _ = dbDateTime backend

instance ToSql DBText where
  toSql backend _ = dbText backend

instance ToSql (DBType t c) where
  toSql backend (Type t)    = toSql backend t
  toSql backend (NotNull t) = showf (String % " NOT NULL") (toSql backend t)

instance ToSql (DBField t c) where
  toSql backend (name := t) = showf (name % " " % String) (toSql backend t)

instance ToSql DBTableField where
  toSql backend (F f) = toSql backend f

instance ToSql DBTable where
  toSql backend
        (DBTable name (map (toSql backend) -> fs)
                      (map (toSql backend) -> cs)) = unlines sql
    where header = showf ("CREATE TABLE " % name)
          defs   = pairs $ intersperse "," $ map ('\t' : ) (fs ++ cs)
          defs'  = map concat defs
          sql    = [header, "("] ++ defs' ++ [");"]

          pairs []           = []
          pairs [x]          = [[x]]
          pairs (x : y : xs) = [x, y] : pairs xs

class ToHaskell a where
  toHaskell :: a -> String

  imports :: a -> [String]
  imports _ = []

instance ToHaskell DBInt where
  toHaskell _ = "Integer"

instance ToHaskell DBText where
  toHaskell _ = "String"

instance ToHaskell DBDateTime where
  toHaskell _ = "DateTime"
  imports   _ = ["Data.DateTime"]

instance ToHaskell (DBType t c) where
  toHaskell (Type t)           = showf ("Maybe " % toHaskell t)
  toHaskell (NotNull (Type t)) = toHaskell t

  imports (Type t)    = imports t
  imports (NotNull t) = imports t

instance ToHaskell (DBField t c) where
  toHaskell (name := t) = showf ("_" % name % " :: " % toHaskell t)
  imports   (_ := t)    = imports t

instance ToHaskell DBTableField where
  toHaskell (F f) = toHaskell f
  imports   (F f) = imports f

instance ToHaskell DBTable where
  toHaskell table@(DBTable _ fields _) =
    unlines $ (dataType : dbModel : accessors)
    where fs       = map toHaskell fields
          typeName = normalizedName table
          fNames   = map name fields

          dataType = renderf T.dataType ("tableType", typeName)
                                        ("fields",    fs)

          accessors = map (uncurry renderAccessor) $ zip fNames types
            where renderAccessor name rType =
                    renderf T.accessor ("accessor", name)
                                       ("table",    typeName)
                                       ("type",     rType)
                  dbType (F (_ := t)) = showf ("( " % toHaskell t % " )")
                  types = map dbType fields

          dbModel = renderf T.dbModelInstance ("tableType",   typeName)
                                              ("cardinality", length fNames)
                                              ("tableName",   name table)
                                              ("fromSqls",    fromSqls)
                                              ("toSqls",      toSqls)
                                              ("undefined",   us)
                                              ("fieldNames",  names)
            where us       = map (const "undefined") fNames
                  fromSqls = map (showf ("(fromSql _" % String % ")")) fNames
                  toSqls   = map (showf ("(toSql $ _" % String % " t)")) fNames
                  names    = map ('_':) fNames

  imports (DBTable _ fs _) =
    unique $ sort $ concatMap imports fs ++ ["DB.Models", "Database.HDBC"]
    where unique = map head . group

spliceHaskell :: [DBTable] -> FilePath -> IO [FilePath]
spliceHaskell db dir = do
  exists <- doesDirectoryExist models
  when exists $
    removeDirectoryRecursive models

  createDirectoryIfMissing True models

  forM (map renderTable db) $ \(file, content) -> do
    writeFile file content
    return file

  where models = dir </> "Models"

        renderTable :: DBTable -> (FilePath, String)
        renderTable table@(DBTable _ fields _) =
          (models </> fileName, content)
          where tableType    = normalizedName table
                accessors    = map name fields

                moduleHeader = renderf T.moduleHeader ("tableType", tableType)
                                                      ("accessors", accessors)
                                                      ("prefix",    "DB.")

                importList = renderf T.imports ("imports", importList')
                  where importList'  = map renderImport (imports table)
                        renderImport = showf ("import " % String)

                fileName = showf (tableType % ".hs")
                content  = unlines [moduleHeader, importList, toHaskell table]
