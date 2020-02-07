{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}

module Data.List.Cons where

import GHC.TypeLits
import Data.Kind

import Data.Singletons
import Data.Singletons.Prelude.List (Sing(..))

import Data.Constraint.HasDict1
import Data.SOP.Map

type instance MapOut ("Cons" :# x) (xs :: Type) = MapOutCons x xs

-- | Replaces types of the form @[[a]]@ with @a@ and otherwise fails
-- with a `TypeError`
type family MapOutCons (x :: a) (xs :: Type) :: Type where
  MapOutCons (_ :: a) [a] = [a]
  MapOutCons _ t = TypeError ('Text "MapOutCons (a :: Type) expects: [a], but found: " ':<>: 'ShowType t)

type family Cons (x :: a) (xs :: [a]) :: [a] where
  Cons x xs = x ': xs

instance forall (a :: Type) (x :: a). (HasDict1 a, SingI x) => HasMap ("Cons" :# x) [a] where
  type Mapper ("Cons" :# x) (xs :: [a]) = Cons x xs
  singMapper = SCons sing

