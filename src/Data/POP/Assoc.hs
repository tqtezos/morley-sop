{-# OPTIONS -Wno-missing-export-lists #-}

module Data.POP.Assoc where

import Data.Kind
import Prelude (($))

import Data.Singletons.Prelude.List (Sing(..))
import Data.SOP (POP(..), NP(..))
import qualified Data.SOP as SOP

import Data.List.Concat
import Data.NP.Append

-- | Associate `POP` into `NP`
assocPOP :: forall a (f :: a -> Type) (xs :: [[a]]). ()
  => Sing xs
  -> POP f xs
  -> NP f (Concat xs)
assocPOP SNil (POP _) = SOP.Nil
assocPOP (SCons _ sxs) (POP ((SOP.:*) ps pop)) =
  appendNP ps $
  assocPOP sxs $
  POP pop

-- | Un-associate `NP` into `POP`
unAssocPOP :: forall a (f :: a -> Type) (xs :: [[a]]). ()
  => Sing xs
  -> NP f (Concat xs)
  -> POP f xs
unAssocPOP SNil _ = POP SOP.Nil
unAssocPOP (SCons (sx :: Sing x') (sxs :: Sing xs')) ps = POP $
  case unAppendNP @a @f @x' @(Concat xs') sx ps of
    ~(p, ps') ->
      p SOP.:* SOP.unPOP (unAssocPOP sxs ps')

