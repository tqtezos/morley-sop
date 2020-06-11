{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Control.AltError where

import Control.Applicative
import Data.Bool
import Data.Function
import Data.Kind
import Data.Eq
import Data.List
import Data.String
import Text.Show

import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Function
import Data.Singletons.Prelude.Show
import Data.Singletons.Prelude.List
import Data.Singletons.TH

$(singletons [d|
  class Applicative f => AltError (str :: Type) (f :: Type -> Type) | f -> str where
    (<||>) :: Show a => f a -> f a -> f a
    altErr :: str -> f a -- TODO: rename to altError
    altFail :: str -> f a

  throwAlt :: forall str f a. AltError str f => Bool -> str -> f a
  throwAlt False = altErr
  throwAlt True = altFail

  |])

-------------------
-- Combining errors
-------------------

-- | `String` tagging multiple errors
multipleErrors :: String
multipleErrors = "multiple errors:"

-- | Collect multiple errors (the result of `mergeErrors`
unMultipleErrors :: [String] -> [String]
unMultipleErrors [] = []
unMultipleErrors (z:zs) =
  bool_ (z :) id (z == multipleErrors) zs

-- | Merge two lisy of errors
mergeErrors :: [String] -> [String] -> [String]
mergeErrors x y =
  multipleErrors : (
    unMultipleErrors x ++
    unMultipleErrors y)

-- | Utility to combine two instances of `throwAlt`
--
-- @
--  combineThrowAlt isFailXs errXs isFailYs errYs =
-- @
combineThrowAlt :: AltError [String] f => Bool -> [String] -> Bool -> [String] -> f a
combineThrowAlt isFailXs errXs isFailYs errYs =
  throwAlt (isFailXs || isFailYs) (mergeErrors errXs errYs)

---------------------------------------
-- Singleton (Symbol): Combining errors
---------------------------------------

$(singletonsOnly [d|

  multipleErrors :: Symbol
  multipleErrors = "multiple errors:"

  unMultipleErrors :: [Symbol] -> [Symbol]
  unMultipleErrors [] = []
  unMultipleErrors (z:zs) =
    bool_ (z :) id (z == multipleErrors) zs

  mergeErrors :: [Symbol] -> [Symbol] -> [Symbol]
  mergeErrors x y =
    multipleErrors : (
      unMultipleErrors x ++
      unMultipleErrors y)

  combineThrowAlt :: AltError [Symbol] f => Bool -> [Symbol] -> Bool -> [Symbol] -> f a
  combineThrowAlt isFailXs errXs isFailYs errYs =
    throwAlt (isFailXs || isFailYs) (mergeErrors errXs errYs)

  |])

