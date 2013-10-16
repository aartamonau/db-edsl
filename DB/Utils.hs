{-# LANGUAGE GADTs, EmptyDataDecls #-}

module DB.Utils
       where

data Empty
data NonEmpty

infixr 5 :>>
data List c a where
  Nil   :: List Empty a
  (:>>) :: a -> List c a -> List NonEmpty a

type NonEmptyList a = List NonEmpty a

toList :: List c a -> [a]
toList Nil        = []
toList (x :>> xs) = x : toList xs

test = 5 :>> 4 :>> Nil