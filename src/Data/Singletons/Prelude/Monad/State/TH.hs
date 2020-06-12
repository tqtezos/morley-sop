{-# OPTIONS -Wno-missing-export-lists -Wno-unused-type-patterns #-}

-- | See `Data.Singletons.Prelude.Monad.State` for documentation
module Data.Singletons.Prelude.Monad.State.TH where

import Data.Function
import Data.Tuple

import Data.Singletons.Prelude
import Data.Singletons.TH

import Data.Singletons.Prelude.Monad.State

$(genPromotions [''State'])

$(singletonsOnly [d|
  evalState' :: State' s a -> s -> a
  evalState' m s = fst (m s)

  (<$$>) :: (a -> b) -> State' s a -> State' s b
  (<$$>) f m = (\ x -> (f (evalState' m x), snd (m x)))

  pureState' :: a -> State' s a
  pureState' = (,)

  (<<*>>) :: State' s (a -> b) -> State' s a -> State' s b
  (<<*>>) mf mx
    = (\ x -> let (fs, y) = mf x in let (xs, z) = mx y in (fs xs, z))

  (*>>) :: State' s a -> State' s b -> State' s b
  (*>>) xs ys = (const id <$$> xs) <<*>> ys

  (>>>=) :: State' s a -> (a -> State' s b) -> State' s b
  (>>>=) m f = (\ x -> let (a, y) = m x in f a y)

  getState' :: State' s s
  getState' = (\ x -> (x, x))

  putState' :: s -> State' s ()
  putState' x = (\ _y -> ((), x))

  |])


