{-# OPTIONS -Wno-missing-export-lists #-}

module Data.Constraint.HasDict1 where

import Prelude (($))
import Data.Either
import Data.List.NonEmpty (NonEmpty(..))

import Data.Constraint
import Data.Singletons
import Data.Singletons.TypeLits (Sing(..), Symbol)
import Data.Singletons.Prelude.List (Sing(..))
import Data.Singletons.Prelude.List.NonEmpty (Sing(..))
import Data.Singletons.Prelude.Tuple
import Data.Singletons.Prelude.Either

import Data.Singletons.TH.Cases
import Language.Haskell.TH.Syntax

-- | Proof that `Sing` entails `SingI`
class HasDict1 a where
  evidence1 :: forall (x :: a). Sing x -> Dict (SingI x)

withDict1 :: forall a (x :: a) r. HasDict1 a => Sing x -> (SingI x => r) -> r
withDict1 x = withDict (evidence1 x)

-- | Generate `evidence1` for a data type
gen_evidence1 :: Name -> Q Exp
gen_evidence1 = sCasesFold [| withDict1 |] [| Dict |]

instance HasDict1 a => HasDict1 [a] where
  evidence1 = $(sCasesFold [| withDict1 |] [| Dict |] ''[])

instance (HasDict1 a, HasDict1 b) => HasDict1 (a, b) where
  evidence1 = $(sCasesFold [| withDict1 |] [| Dict |] ''(,))

instance HasDict1 a => HasDict1 (NonEmpty a) where
  evidence1 = $(sCasesFold [| withDict1 |] [| Dict |] ''NonEmpty)

instance HasDict1 Symbol where
  evidence1 SSym = Dict -- SSym isn't picked up by sCasesFold

instance (HasDict1 a, HasDict1 b) => HasDict1 (Either a b) where
  evidence1 = $(sCasesFold [| withDict1 |] [| Dict |] ''Either)

