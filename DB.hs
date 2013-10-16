{-# LANGUAGE GeneralizedNewtypeDeriving,
             FlexibleContexts, ScopedTypeVariables, EmptyDataDecls,
             ViewPatterns #-}

module DB
       where

import Control.Monad (mapM_)

import Data.List (intersperse)
import Data.Monoid
import Text.XFormat.Show

import Data.Convertible
import qualified Database.HDBC as DB

import DB.Models (DBModel (tableName, fromSqlValues,
                           toSqlValues, rowCardinality),
                  Accessor,
                  Assignment (..), TypedAssignment (..), name)
import DB.Utils

data Constraint t = Constraint (String -> String) [DB.SqlValue]
newtype Query t = Query [Constraint t]
                deriving Monoid

notNull :: Accessor t (Maybe r) -> Query t
notNull accessor = Query [constraint]
  where constraint =
          Constraint (\t -> showf (name accessor t % " IS NOT NULL")) []

comparison :: (Ord r, Convertible r DB.SqlValue) =>
              String -> Accessor t r -> r -> Query t
comparison op accessor v = Query [constraint]
  where constraint =
          Constraint (\t -> showf (name accessor t % " " % op % " ?"))
                     [DB.toSql v]

(<:) :: (Ord r, Convertible r DB.SqlValue) => Accessor t r -> r -> Query t
(<:) = comparison "<"

(>:) :: (Ord r, Convertible r DB.SqlValue) => Accessor t r -> r -> Query t
(>:) = comparison ">"

(=:) :: (Ord r, Convertible r DB.SqlValue) => Accessor t r -> r -> Query t
(=:) = comparison "="

select :: [Query t] -> Query t
select = mconcat

constraintsToSql :: [Constraint t] -> (String -> String, [DB.SqlValue])
constraintsToSql constraints = (cs, concatMap params constraints)
  where cs table =
          concat $ intersperse " AND " $ map (wrap '(' ')' . cToStr table) constraints
          where cToStr t (Constraint f _) = f t
                wrap a b s = a : s ++ [b]
        params (Constraint _ ps) = ps

queryToSql :: forall t . DBModel t => Query t -> (String, [DB.SqlValue])
queryToSql (Query constraints)
  | null constraints = (sql', [])
  | otherwise        = (sql, params)
  where table  = tableName (undefined :: t)
        sql'   = showf ("SELECT * FROM " % table)

        (cs, params) = constraintsToSql constraints
        sql = showf (sql' % " WHERE " % cs (showf (table % ".")) )

runQuery :: (DB.IConnection c, DBModel t) => c -> Query t -> IO [t]
runQuery conn query = fmap (map fromSqlValues) $ DB.quickQuery' conn sql params
  where (sql, params) = queryToSql query

data Update t =
  Update (Query t) [(String -> String, DB.SqlValue)]

update :: [Query t] -> NonEmptyList (Assignment t) -> Update t
update qs as = Update (mconcat qs) (map asTuple (toList as))
  where asTuple :: Assignment t -> (String -> String, DB.SqlValue)
        asTuple (A a@(acc := _)) = (\t -> name acc t, DB.toSql a)

updateToSql :: forall t . DBModel t => Update t -> (String, [DB.SqlValue])
updateToSql (Update (Query constraints) as)
  | null constraints = (sql', params')
  | otherwise        = (sql, params' ++ params)
    where table = tableName (undefined :: t)
          sql'  = showf ("UPDATE " % table % " SET " % updates)
          sql   = showf (sql' % " WHERE " % cs "")

          (cs, params) = constraintsToSql constraints

          updates = concat $ intersperse ", " $ map renderAssignment as
            where renderAssignment (nameF, _) =
                    showf (nameF "" % " = ?")
          params' = map snd as

runUpdate :: (DB.IConnection c, DBModel t) => c -> Update t -> IO Integer
runUpdate conn statement =
  DB.withTransaction conn $ \conn ->
    DB.run conn sql params

  where (sql, params) = updateToSql statement

data Insert t =
  Insert [[DB.SqlValue]]

insert :: DBModel t => NonEmptyList t -> Insert t
insert = Insert . map toSqlValues . toList

insertToSql :: forall t . DBModel t => Insert t -> [(String, [DB.SqlValue])]
insertToSql (Insert vs) =
  zip (repeat sql) vs
  where table = tableName (undefined :: t)
        cardinality = rowCardinality (undefined :: t)

        sql = showf ("INSERT INTO " % table % " VALUES (" % holders % ")")
          where holders = concat $ intersperse ", " $ replicate cardinality "?"

runInsert :: (DB.IConnection c, DBModel t) => c -> Insert t -> IO ()
runInsert conn insert =
  DB.withTransaction conn $ \conn ->
    mapM_ (uncurry $ DB.run conn) sql

  where sql = insertToSql insert
