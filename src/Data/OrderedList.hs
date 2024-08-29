module Data.OrderedList where

import Data.List (sort)

import Data.Functor.Coyoneda

import Control.Applicative.Free

import Data.List.ApplyMerge

data OrderedList a where
  OrderedList :: Ord a => [a] -> OrderedList a

orderedList :: Ord a => [a] -> OrderedList a
orderedList = OrderedList . sort

-- | Only use if list is already sorted
unsafeOrderedList :: Ord a => [a] -> OrderedList a
unsafeOrderedList = OrderedList

runOrderedList :: OrderedList a -> [a]
runOrderedList (OrderedList as) = as

newtype COrderedList a = COrderedList { getCOrderedList :: Coyoneda OrderedList a }
  deriving Functor

cOrderedList :: Ord a => [a] -> COrderedList a
cOrderedList = COrderedList . liftCoyoneda . orderedList

{-
instance Applicative COrderedList where
  pure a = COrderedList $ Coyoneda (const a) $ orderedList [()]
  COrderedList (Coyoneda f (OrderedList as)) <*> COrderedList (Coyoneda g (OrderedList bs)) = COrderedList $ Coyoneda
    _
    $ OrderedList $ applyMerge (\a' b' -> f a' $ g b') as bs

-}

newtype FOrderedList a = FOrderedList { getFOrderedList :: Ap OrderedList a }
  deriving (Functor, Applicative)

runFOrderedList :: FOrderedList a -> [a]
runFOrderedList = runFOrderedList' . getFOrderedList
  where
    runFOrderedList' :: Ap OrderedList a -> [a]
    runFOrderedList' (Pure a) = [a]
    runFOrderedList' (Ap as (Pure f)) = f <$> runOrderedList as
    -- runFOrderedList' (Ap as (Ap bs x)) = _
    -- Maybe I need to roll my own applicative

{-
data FOrderedList2 a where
  Ordered :: OrderedList a -> FOrderedList2 a
  Single :: a -> FOrderedList2 a
  ApFO :: Ord b => OrderedList a -> FOrderedList2 (a -> b) -> FOrderedList2 b

instance Functor FOrderedList2 where
  fmap f (Ordered as) = ApFO as (Single f)
  fmap f (Single a) = Single $ f a
  fmap f (ApFO a g) = ApFO a $ fmap (f .) g

instance Applicative FOrderedList2 where
  pure = Single
  Single f <*> Single a = Single $ f a
  f <*> Ordered as = ApFO as f
  -- ApFO as (Single f) <*> Ordered bs = Ordered $ unsafeOrderedList $ applyMerge f (runOrderedList as) (runOrderedList bs)
-}

data FOrderedList3 a where
  Ordered3 :: OrderedList a -> FOrderedList3 a
  ApplyMerge :: Ord c => (a -> b -> c) -> FOrderedList3 a -> FOrderedList3 b -> FOrderedList3 c

runFOrderedList3 :: FOrderedList3 a -> OrderedList a
runFOrderedList3 (Ordered3 as) = as
runFOrderedList3 (ApplyMerge f as bs) = unsafeOrderedList $ applyMerge f (runOrderedList $ runFOrderedList3 as) (runOrderedList $ runFOrderedList3 bs)

newtype COrderedList3 a = COrderedList3 { getCOrderedList3 :: Coyoneda FOrderedList3 a }
  deriving Functor

instance Applicative (COrderedList3) where
  pure a = COrderedList3 $ Coyoneda (const a) $ Ordered3 $ unsafeOrderedList [()]
  Coyoneda f (Ordered3 as) <*> Coyoneda g (Ordered3 bs) = Coyoneda _ _ _
  -- Ordered3 f as <*> Ordered3 g bs = ApplyMerge _ as bs
