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

-- | Proof that `Sing` entails `SingI`
class HasDict1 a where
  evidence1 :: forall (x :: a). Sing x -> Dict (SingI x)

withDict1 :: forall a (x :: a) r. HasDict1 a => Sing x -> (SingI x => r) -> r
withDict1 x = withDict (evidence1 x)

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

instance (HasDict1 a, HasDict1 b) => HasDict1 (Either a b) where
  evidence1 (SLeft xs) =
    withDict (evidence1 xs) $
    Dict
  evidence1 (SRight xs) =
    withDict (evidence1 xs) $
    Dict

-- -- | Warning: not total; fails on `SShowType`
-- instance HasDict1 (ErrorMessage' Symbol) where
--   evidence1 (SText xs) =
--     withDict (evidence1 xs) $
--     Dict
--   evidence1 (SShowType xs) = error "evidence1 (SShowType _)"
--     -- withDict (evidence1 xs) $
--     -- Dict
--   evidence1 ((:%<>:) xs ys) =
--     withDict (evidence1 xs) $
--     withDict (evidence1 ys) $
--     Dict
--   evidence1 ((:%$$:) xs ys) =
--     withDict (evidence1 xs) $
--     withDict (evidence1 ys) $
--     Dict

