{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-type-patterns #-}

module Data.Singletons.Map where

import Prelude

-- | A map from @k@ to @a@ implement using a list
newtype ListMap k a = MkListMap
  { unListMap :: [(k, a)]
  } deriving (Show, Functor)

-- | An empty `ListMap`
emptyListMap :: ListMap k a
emptyListMap = MkListMap []

-- | Lookup and modify a value, given a default value if it's not in the map.
lookupModifyListMap :: Ord k => a -> (a -> (b, a)) -> k -> ListMap k a -> (b, ListMap k a)
lookupModifyListMap x0 f key (MkListMap []) = (\y -> MkListMap [(key, y)]) <$> f x0
lookupModifyListMap x0 f key (MkListMap ((k, x):xs)) =
  case compare key k of
    LT -> (\(MkListMap y) -> MkListMap ((k, x) : y)) <$> lookupModifyListMap x0 f key (MkListMap xs)
    EQ -> (\y -> MkListMap ((key, y) : xs)) <$> f x
    GT -> (\y -> MkListMap ((key, y) : (k, x) : xs)) <$> f x0

