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

import Data.Singletons.Prelude.Bool

class Applicative f => AltError (str :: Type) (f :: Type -> Type) | f -> str where
  (<||>) :: forall a. Show a => f a -> f a -> f a
  altErr :: forall a. str -> f a -- TODO: rename to altError
  altFail :: forall a. str -> f a

-- | `altFail` if `True`, `altErr` otherwise
throwAlt :: forall str f a. AltError str f => Bool -> str -> f a
throwAlt False = altErr
throwAlt True = altFail

-------------------
-- Combining errors
-------------------

-- `String` tagging multiple errors
multipleErrors :: forall s. IsString s => s
multipleErrors = "multiple errors:"

-- Collect multiple errors (the result of `mergeErrors`
unMultipleErrors :: forall s. (IsString s, Eq s) => [s] -> [s]
unMultipleErrors [] = []
unMultipleErrors (z:zs) =
  bool_ (z :) id (z == multipleErrors) zs

-- Merge two lisy of errors
mergeErrors :: forall s. (IsString s, Eq s) => [s] -> [s] -> [s]
mergeErrors x y =
  multipleErrors : (
    unMultipleErrors x ++
    unMultipleErrors y)

-- Utility to combine two instances of `throwAlt`
--
-- @
--  combineThrowAlt isFailXs errXs isFailYs errYs =
-- @
combineThrowAlt :: forall s f a. (IsString s, Eq s, AltError [s] f) => Bool -> [s] -> Bool -> [s] -> f a
combineThrowAlt isFailXs errXs isFailYs errYs =
  throwAlt (isFailXs || isFailYs) (mergeErrors errXs errYs)

