{-# OPTIONS -Wno-missing-export-lists #-}

module Data.List.Append where

import Prelude (($))

import Data.Constraint
import Data.Singletons
import Data.Singletons.Prelude.List (Sing(..))

import Data.Constraint.HasDict1

-- | Append two lists
--
-- This definition is more amenable to proofs
-- than the one in Data.Singletons.Prelude.List
type family (++) (xs :: [a]) (ys :: [a]) :: [a] where
  (++) '[] ys = ys
  (++) (x ': xs) ys = x ': (xs ++ ys)

-- | `(++)` preserves `Sing`
singAppend :: forall a (xs :: [a]) (ys :: [a]). Sing xs -> Sing ys -> Sing (xs ++ ys)
singAppend SNil sys = sys
singAppend (SCons sx sxs) sys =
  SCons sx $ singAppend sxs sys

-- | `(++)` preserves `SingI`, given `HasDict1`
singIAppend :: forall a (xs :: [a]) (ys :: [a]). HasDict1 a => (SingI xs, SingI ys) :- SingI (xs ++ ys)
singIAppend = Sub $
  evidence1 $
  singAppend (sing @xs) (sing @ys)
  -- case sing @xs of
  --   SNil -> Dict
  --   SCons sx (sxs :: Sing xs') ->
  --     withDict (evidence1 sx) $
  --     withDict (evidence1 sxs) $
  --     withDict (singIAppend @a @xs' @ys) $
  --     Dict

