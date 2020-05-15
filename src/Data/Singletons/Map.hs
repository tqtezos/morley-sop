{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Data.Singletons.Map where

-- -- import Prelude hiding (unwords, show)
-- import Data.List
-- import Text.Show

-- -- import Control.AltError
-- -- import Data.AltError
-- -- import Data.ListError

-- -- import Michelson.Typed.Annotation.Path
-- -- import Michelson.Typed.EntryPoints.Error

-- -- import Michelson.Typed.Annotation.Sing.Alg
-- -- import Michelson.Typed.T.Alg

-- import Data.Singletons.TH
-- -- import Data.Singletons.TypeLits
-- import Data.Singletons.Prelude
-- -- import Data.Singletons.Prelude.List
-- -- import Data.Singletons.Prelude.Monad
-- -- import Data.Singletons.Prelude.Semigroup
-- -- import Data.Singletons.Prelude.Bool

-- $(singletons [d|
--   newtype ListMap k a = ListMap
--     { unListMap :: [(k, a)]
--     } deriving (Show)

--   lookupModifyListMap :: Ord k => a -> (a -> (a, b)) -> k -> ListMap k a -> (ListMap k a, b)
--   lookupModifyListMap x0 f key (ListMap []) = (ListMap [(key, f x0)], f x0)
--   lookupModifyListMap x0 f key (ListMap ((k, x):xs)) =
--     case compare key k of
--       LT -> ListMap ((k, x) : unListMap (lookupModifyListMap x0 f key (ListMap xs)))
--       EQ -> ListMap ((k, f x) : xs)
--       GT -> ListMap ((k, x) : xs)

--   |])


-- epFields t ann epPath :: [Symbol]






