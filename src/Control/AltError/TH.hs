{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS -Wno-missing-export-lists #-}

-- | See `Control.AltError` for documentation
module Control.AltError.TH where

import Data.Bool
import Data.Function
import Data.Eq
import Data.List
import Data.String

import Control.AltError

import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Function
import Data.Singletons.Prelude.Show
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.IsString
import Data.Singletons.TH

$(genSingletons [''AltError])

$(singletonsOnly [d|

  throwAlt :: forall str f a. AltError str f => Bool -> str -> f a
  throwAlt False = altErr
  throwAlt True = altFail

  ---------------------------------------
  -- Singleton (Symbol): Combining errors
  ---------------------------------------

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

  |])

