{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Data.SOP.MapCons where

import Data.Kind
import Prelude (($), (<$>), (.))

import Data.Singletons.Prelude.List (Sing(..))
import Data.SOP (SOP(..))
import qualified Data.SOP as SOP

import Data.SOP.Map
import Data.List.Cons ()

-- | `Map` @"Cons" :# x@ over `SOP`
mapConsSOP :: forall a (f :: a -> Type) (x :: a) (xs :: [[a]]).
     Sing xs
  -> f x
  -> SOP f xs
  -> SOP f (Map ("Cons" :# x) xs)
mapConsSOP SNil _ sop = sop
mapConsSOP (SCons _ sys) fx (SOP sop) = SOP $
  case sop of
    SOP.Z ps ->
      SOP.Z $
      fx SOP.:* ps
    SOP.S sop' ->
      SOP.S $
      SOP.unSOP $
      mapConsSOP sys fx $ SOP sop'

-- | Un-`Map` @"Cons" :# x@ over `SOP`
unMapConsSOP :: forall a (f :: a -> Type) (x :: a) (xs :: [[a]]).
     Sing x
  -> Sing xs
  -> SOP f (Map ("Cons" :# x) xs)
  -> (f x, SOP f xs)
unMapConsSOP _ SNil (SOP sop) =
  case sop of
unMapConsSOP sx (SCons _ sys) (SOP sop) =
  case sop of
    SOP.Z ((SOP.:*) p ps) ->
      (p, SOP $ SOP.Z ps)
    SOP.S sop' ->
      SOP . SOP.S . SOP.unSOP <$>
      unMapConsSOP sx sys (SOP sop')

