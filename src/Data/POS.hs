{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Data.POS where

import Data.Either
import Data.Bifunctor
import Data.Kind
import Prelude (($), flip)

import Data.Constraint
import Data.Singletons.Prelude.List (Sing(..))
import Data.SOP (SOP(..), NS(..), NP(..))
import qualified Data.SOP as SOP

import Data.Constraint.HasDict1
import Data.List.Distribute
import Data.NS.Append
import Data.SOP.Map
import Data.SOP.MapCons

-- | A product of sums
newtype POS (f :: a -> Type) (xs :: [[a]]) where
  POS :: forall a (f :: a -> Type) (xs :: [[a]]).
    { unPOS :: NP (NS f) xs }
    -> POS f xs

-- | Distribute a `POS`, inverse to `unDistributePOS`
distributePOS :: forall a (f :: a -> Type) (xs :: [[a]]). (HasDict1 a)
  => Sing xs
  -> POS f xs
  -> SOP f (Distribute xs)
distributePOS SNil _ = SOP $
  SOP.Z SOP.Nil
distributePOS (SCons sx sxs) (POS ((SOP.:*) ss pos)) =
  distributeConsPOS sx sxs ss $
  POS pos

-- | Un-distribute a `POS`, inverse to `distributePOS`
unDistributePOS :: forall a (f :: a -> Type) (xs :: [[a]]). (HasDict1 a)
  => Sing xs
  -> SOP f (Distribute xs)
  -> POS f xs
unDistributePOS SNil _ = POS $
  SOP.Nil
unDistributePOS (SCons sx sxs) sop = POS $
  case unDistributeConsPOS sx sxs sop of
    ~(ss, POS pos) ->
      ss SOP.:* pos

-- | One step of `distributePOS`
distributeConsPOS :: forall a (f :: a -> Type) (xs :: [a]) (xss :: [[a]]). (HasDict1 a)
  => Sing xs
  -> Sing xss
  -> NS f xs
  -> POS f xss
  -> SOP f (DistributeCons xs xss)
distributeConsPOS SNil _ ns _ =
  case ns of
distributeConsPOS (SCons _ sxs) sxss (SOP.Z ns) (POS pos) = SOP $
  flip postpendNS (singDistributeCons sxs sxss) $
  SOP.unSOP $
  mapConsSOP (singDistribute sxss) ns $
  distributePOS sxss $
  POS pos
distributeConsPOS (SCons (sx :: Sing x) sxs) sxss (SOP.S ns) (POS pos) = SOP $
  withDict (evidence1 sx) $
  prependNS (singMap @("Cons" :# x) (singDistribute sxss)) $
  SOP.unSOP $
  distributeConsPOS sxs sxss ns $
  POS pos

-- | One step of `unDistributePOS`
unDistributeConsPOS :: forall a (f :: a -> Type) (xs :: [a]) (xss :: [[a]]). (HasDict1 a)
  => Sing xs
  -> Sing xss
  -> SOP f (DistributeCons xs xss)
  -> (NS f xs, POS f xss)
unDistributeConsPOS SNil _ (SOP sop) =
  case sop of
unDistributeConsPOS (SCons (sx :: Sing x) sxs) sxss (SOP sop) =
  withDict (evidence1 sx) $
  case unAppendNS (singMap @("Cons" :# x) (singDistribute sxss)) (singDistributeCons sxs sxss) sop of
    Left sop' ->
      case unMapConsSOP sx (singDistribute sxss) $ SOP sop' of
        ~(fx, fDistributeXss) ->
          (SOP.Z fx, unDistributePOS sxss fDistributeXss)
    Right sop' ->
      first SOP.S $
      unDistributeConsPOS sxs sxss $
      SOP sop'

