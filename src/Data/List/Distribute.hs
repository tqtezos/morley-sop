{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}

module Data.List.Distribute where

import Data.Kind
import Prelude (($))

import Data.Constraint
import Data.Singletons.Prelude.List (Sing(..))

import Data.List.Append
import Data.Constraint.HasDict1
import Data.List.Cons ()
import Data.SOP.Map

-- | Distribute a product over a sum
type family Distribute (xs :: [[a]]) :: [[a]] where
  Distribute '[] = '[ '[]]
  Distribute (x ': xs) = DistributeCons x xs

-- | `Distribute` preserves `Sing`
singDistribute :: forall a (xs :: [[a]]). HasDict1 a =>
  Sing xs -> Sing (Distribute xs)
singDistribute SNil = SCons SNil SNil
singDistribute (SCons sx sxs) =
  singDistributeCons sx sxs

-- | The non-empty case for `Distribute`
type family DistributeCons (xs :: [a]) (xss :: [[a]]) :: [[a]] where
  -- sum [] * _ xss = sum []
  DistributeCons '[] _xss = '[]

  DistributeCons (x ': xs) xss =
    Map ("Cons" :# x) (Distribute xss) ++
    DistributeCons xs xss

-- | `DistributeCons` preserves `Sing`
singDistributeCons :: forall a (xs :: [a]) (xss :: [[a]]). HasDict1 a =>
  Sing xs -> Sing xss -> Sing (DistributeCons xs xss)
singDistributeCons SNil _ = SNil
singDistributeCons (SCons (sx :: Sing x) sxs) sxss =
  withDict (evidence1 sx) $
  singMap @("Cons" :# x) (singDistribute sxss) `singAppend`
  singDistributeCons sxs sxss

type instance MapOut ("Distribute" :# '()) [[a]] = [[a]]

instance forall (a :: Type). HasDict1 a => HasMap ("Distribute" :# '()) [[a]] where
  type Mapper ("Distribute" :# '()) (xs :: [[a]]) = Distribute xs
  singMapper = singDistribute

