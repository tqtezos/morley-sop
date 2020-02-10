{-# OPTIONS -Wno-missing-export-lists #-}

module Data.Constraint.HasDict1 where

import Prelude (($))
import Data.List.NonEmpty (NonEmpty(..))

import Data.Constraint
import Data.Singletons
import Data.Singletons.TypeLits (Sing(..), Symbol)
import Data.Singletons.Prelude.List (Sing(..))
import Data.Singletons.Prelude.List.NonEmpty (Sing(..))
import Data.Singletons.Prelude.Tuple

-- | Proof that `Sing` entails `SingI`
class HasDict1 a where
  evidence1 :: forall (x :: a). Sing x -> Dict (SingI x)

instance HasDict1 a => HasDict1 [a] where
  evidence1 SNil = Dict
  evidence1 (SCons sx sxs) =
    withDict (evidence1 sx) $
    withDict (evidence1 sxs) $
    Dict

instance (HasDict1 a, HasDict1 b) => HasDict1 (a, b) where
  evidence1 (STuple2 sx sy) =
    withDict (evidence1 sx) $
    withDict (evidence1 sy) $
    Dict

instance HasDict1 a => HasDict1 (NonEmpty a) where
  evidence1 ((:%|) sx sxs) =
    withDict (evidence1 sx) $
    withDict (evidence1 sxs) $
    Dict

instance HasDict1 Symbol where
  evidence1 SSym = Dict

