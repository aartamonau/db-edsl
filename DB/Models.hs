{-# LANGUAGE ExistentialQuantification, FlexibleInstances,
             MultiParamTypeClasses, FlexibleContexts #-}

module DB.Models
       where

import Data.Convertible
import qualified Database.HDBC as DB

class DBModel t where
  empty :: t
  tableName :: t -> String
  rowCardinality :: t -> Int

  get :: t -> Accessor t r -> r
  get table accessor = (getter accessor) table

  set :: t -> Accessor t r -> r -> t
  set table accessor v = (setter accessor) table v

  create :: [Assignment t] -> t
  create = foldr set' empty
    where set' (A (acc := v)) t = set t acc v

  update :: t -> [Assignment t] -> t
  update = foldr set'
    where set' (A (acc := v)) t = set t acc v

  fromSqlValues :: [DB.SqlValue] -> t
  toSqlValues   :: t -> [DB.SqlValue]

data Accessor t r =
  Accessor { name   :: String -> String
           , getter :: t -> r
           , setter :: t -> r -> t
           }

data TypedAssignment t r = (Accessor t r) := r
data Assignment t =
  forall r . Convertible r DB.SqlValue => A (TypedAssignment t r)

instance Convertible r DB.SqlValue =>
         Convertible (TypedAssignment t r) DB.SqlValue where
  safeConvert (_ := v) = safeConvert v
instance Convertible (Assignment t) DB.SqlValue where
  safeConvert (A a) = safeConvert a