module Data.OrderedList where

import Data.List (sort)

import Data.Functor.Coyoneda

import Data.List.ApplyMerge

data OrderedList a where
  OrderedList :: Ord a => [a] -> OrderedList a

orderedList :: Ord a => [a] -> OrderedList a
orderedList = OrderedList . sort

-- | Only use if list is already sorted
unsafeOrderedList :: Ord a => [a] -> OrderedList a
unsafeOrderedList = OrderedList

newtype COrderedList a = COrderedList { getCOrderedList :: Coyoneda OrderedList a }
  deriving Functor

cOrderedList :: Ord a => [a] -> COrderedList a
cOrderedList = COrderedList . liftCoyoneda . orderedList

instance Applicative COrderedList where
  pure a = COrderedList $ Coyoneda (const a) $ orderedList [()]
  COrderedList (Coyoneda f (OrderedList as)) <*> COrderedList (Coyoneda g (OrderedList bs)) = COrderedList $ Coyoneda
    _
    $ OrderedList $ applyMerge (\a' b' -> f a' $ g b') as bs
