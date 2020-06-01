{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Data.Singletons.Map where

import Prelude
import Data.List
import Text.Show

import Data.Singletons.TH
import Data.Singletons.Prelude

$(singletons [d|
  newtype ListMap k a = MkListMap
    { unListMap :: [(k, a)]
    } deriving (Show, Functor)

  emptyListMap :: ListMap k a
  emptyListMap = MkListMap []

  lookupModifyListMap :: Ord k => a -> (a -> (b, a)) -> k -> ListMap k a -> (b, ListMap k a)
  lookupModifyListMap x0 f key (MkListMap []) = (\y -> MkListMap [(key, y)]) <$> f x0
  lookupModifyListMap x0 f key (MkListMap ((k, x):xs)) =
    case compare key k of
      LT -> (\(MkListMap y) -> MkListMap ((k, x) : y)) <$> lookupModifyListMap x0 f key (MkListMap xs)
      EQ -> (\y -> MkListMap ((key, y) : xs)) <$> f x
      GT -> (\y -> MkListMap ((key, y) : (k, x) : xs)) <$> f x0

  |])

