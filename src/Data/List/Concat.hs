{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}

module Data.List.Concat where

import Data.List.Append

import Prelude (($))

import Data.Constraint
import Data.Singletons
import Data.Singletons.Prelude.List (Sing(..))

import Data.Constraint.HasDict1
import Data.SOP.Map

-- | `Concat`enate the elements of a list
type family Concat (xs :: [[a]]) :: [a] where
  Concat '[] = '[]
  Concat (x ': xs) = x ++ Concat xs

-- | `Concat` preserves `Sing`
singConcat :: Sing xs -> Sing (Concat xs)
singConcat SNil = SNil
singConcat (SCons sx sxs) =
  sx `singAppend` singConcat sxs

-- | `Concat` preserves `SingI`
singIConcat :: forall a (xs :: [[a]]). HasDict1 a => SingI xs :- SingI (Concat xs)
singIConcat = Sub $
  evidence1 $
  singConcat (sing @xs)

type instance MapOut ("Concat" :# '()) [[a]] = [a]

instance HasDict1 a => HasMap ("Concat" :# '()) [[a]] where
  type Mapper ("Concat" :# '()) xs = Concat xs
  singMapper = singConcat
  singIMapper = singIConcat

