{-# OPTIONS -Wno-missing-export-lists -Wno-unused-type-patterns #-}

module Data.Singletons.Prelude.Monad.State where

import Data.Function
import Data.Tuple

-- | Pure state monad
type State' s a = s -> (a, s)

-- | Run `State'`, given an initial state
evalState' :: State' s a -> s -> a
evalState' m s = fst (m s)

-- | (`<$>`)
(<$$>) :: (a -> b) -> State' s a -> State' s b
(<$$>) f m = (\x -> (f (evalState' m x), snd (m x)))

-- | `pure`
pureState' :: a -> State' s a
pureState' = (,)

-- | (`<*>`)
(<<*>>) :: State' s (a -> b) -> State' s a -> State' s b
(<<*>>) mf mx = (\x ->
    let (fs, y) = mf x
     in let (xs, z) = mx y
     in (fs xs, z))

-- | (`*>`)
(*>>) :: State' s a -> State' s b -> State' s b
(*>>) xs ys = (const id <$$> xs) <<*>> ys

-- | (`>>=`)
(>>>=) :: State' s a -> (a -> State' s b) -> State' s b
(>>>=) m f  = (\x ->
    let (a, y) = m x
     in f a y)

-- | Get the current state
getState' :: State' s s
getState' = (\x -> (x, x))

-- | Update the current state
putState' :: s -> State' s ()
putState' x = (\_y -> ((), x))

