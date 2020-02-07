{-# OPTIONS -Wno-missing-export-lists #-}

module Data.SOP.Join where

import Data.Kind
import Prelude (($), (.))

import Data.Constraint
import Data.Singletons
import Data.Singletons.Prelude.List (Sing(..))
import Data.SOP (POP(..), SOP(..), NS(..), NP(..), All, Top)
import qualified Data.SOP as SOP

import Data.Constraint.HasDict1
import Data.SOS
import Data.POS
import Data.POP.Assoc
import Data.SOS.Assoc
import Data.SOP.Map

import Data.List.Concat


-- | Map `assocPOP`
mapAssocPOP :: forall a (f :: a -> Type) (xs :: [[[a]]]). (HasDict1 a)
  => Sing xs
  -> NS (POP f) xs
  -> NS (NP f) (Map ("Concat" :# '()) xs)
mapAssocPOP sxs spop =
  ixmap @("Concat" :# '()) @[[a]] assocPOP sxs spop

-- | Map `unAssocPOP`
unMapAssocPOP :: forall a (f :: a -> Type) (xs :: [[[a]]]). (HasDict1 a)
  => Sing xs
  -> NS (NP f) (Map ("Concat" :# '()) xs)
  -> NS (POP f) xs
unMapAssocPOP sxs spop =
  ixunmap @("Concat" :# '()) @[[a]] unAssocPOP sxs spop

-- | A nested sum of products is isomorphic to
-- a sum of a product-of-sums of a product
innerToPOS :: forall a (f :: a -> Type) (xs :: [[[[a]]]]). ()
  => Sing xs
  -> SOP (SOP f) xs
  -> NS (POS (NP f)) xs
innerToPOS sxs (SOP sop) =
  withDict (singAllTop2 sxs) $
  SOP.hcliftA (Proxy @(All Top)) (POS . SOP.hliftA SOP.unSOP) sop

-- | A sum of a product-of-sums of a product is isomorphic to
-- a nested sum of products
innerFromPOS :: forall a (f :: a -> Type) (xs :: [[[[a]]]]). ()
  => Sing xs
  -> NS (POS (NP f)) xs
  -> SOP (SOP f) xs
innerFromPOS sxs psop = SOP $
  withDict (singAllTop2 sxs) $
  SOP.hcliftA (Proxy @(All Top)) (SOP.hliftA SOP . unPOS) psop

-- | Map `distributePOS`
innerDistributePOS :: forall a (f :: a -> Type) (xs :: [[[[a]]]]). (HasDict1 a)
  => Sing xs
  -> NS (POS (NP f)) xs
  -> NS (SOP (NP f)) (Map ("Distribute" :# '()) xs)
innerDistributePOS sxs spos =
  ixmap @("Distribute" :# '()) @[[[a]]] distributePOS sxs spos

-- | Map `unDistributePOS`
innerUnDistributePOS :: forall a (f :: a -> Type) (xs :: [[[[a]]]]). (HasDict1 a)
  => Sing xs
  -> NS (SOP (NP f)) (Map ("Distribute" :# '()) xs)
  -> NS (POS (NP f)) xs
innerUnDistributePOS sxs ssop =
  ixunmap @("Distribute" :# '()) @[[[a]]] unDistributePOS sxs ssop

-- | Expand `SOP` into a sum of products
innerToNSNP :: forall a (f :: a -> Type) (xs :: [[[[a]]]]). (HasDict1 a)
  => Sing xs
  -> NS (SOP (NP f)) (Map ("Distribute" :# '()) xs)
  -> NS (NS (NP (NP f))) (Map ("Distribute" :# '()) xs)
innerToNSNP sxs ssop =
  withDict (singAllTop (singMap @("Distribute" :# '()) sxs)) $
  SOP.hliftA SOP.unSOP ssop

-- | Retract a sum of products into `SOP`
innerFromNSNP :: forall a (f :: a -> Type) (xs :: [[[[a]]]]). (HasDict1 a)
  => Sing xs
  -> NS (NS (NP (NP f))) (Map ("Distribute" :# '()) xs)
  -> NS (SOP (NP f)) (Map ("Distribute" :# '()) xs)
innerFromNSNP sxs ssop =
  withDict (singAllTop (singMap @("Distribute" :# '()) sxs)) $
  SOP.hliftA SOP ssop

-- | Concat the outer `NS` through association (`assocSOS`)
concatNS :: forall a (f :: a -> Type) (xs :: [[[[a]]]]). (HasDict1 a)
  => Sing xs
  -> NS (NS (NP (NP f))) (Map ("Distribute" :# '()) xs)
  -> NS (NP (NP f)) (Concat (Map ("Distribute" :# '()) xs))
concatNS sxs sos =
  assocSOS (singMap @("Distribute" :# '()) sxs) $
  SOS sos

-- | Un-concat the outer `NS` through un-association (`unAssocSOS`)
unConcatNS :: forall a (f :: a -> Type) (xs :: [[[[a]]]]). (HasDict1 a)
  => Sing xs
  -> NS (NP (NP f)) (Concat (Map ("Distribute" :# '()) xs))
  -> NS (NS (NP (NP f))) (Map ("Distribute" :# '()) xs)
unConcatNS sxs sos =
  unSOS $
  unAssocSOS (singMap @("Distribute" :# '()) sxs) sos

-- | Concat the inner `NP`'s through association (`mapAssocPOP`)
concatNP :: forall a (f :: a -> Type) (xs :: [[[[a]]]]). (HasDict1 a)
  => Sing xs
  -> NS (NP (NP f)) (Concat (Map ("Distribute" :# '()) xs))
  -> NS (NP f) (Map ("Concat" :# '()) (Concat (Map ("Distribute" :# '()) xs)))
concatNP sxs spop =
  mapAssocPOP (singConcat (singMap @("Distribute" :# '()) sxs)) $
  withDict (singAllTop (singConcat (singMap @("Distribute" :# '()) sxs))) $
  SOP.hliftA POP spop

-- | Un-concat the inner `NP`'s through association (`unMapAssocPOP`)
unConcatNP :: forall a (f :: a -> Type) (xs :: [[[[a]]]]). (HasDict1 a)
  => Sing xs
  -> NS (NP f) (Map ("Concat" :# '()) (Concat (Map ("Distribute" :# '()) xs)))
  -> NS (NP (NP f)) (Concat (Map ("Distribute" :# '()) xs))
unConcatNP sxs sop =
  withDict (singAllTop (singConcat (singMap @("Distribute" :# '()) sxs))) $
  SOP.hliftA SOP.unPOP $
  unMapAssocPOP (singConcat (singMap @("Distribute" :# '()) sxs)) sop

-- | Join nested `SOP`'s
--
-- @
--  | Join nested `SOP`'s
--
-- @
--  \sxs ->
--  `SOP` .
--  `concatNP` sxs .
--  `concatNS` sxs .
--  `innerToNSNP` sxs .
--  `innerDistributePOS` sxs .
--  `innerToPOS` sxs
-- @
joinSOP :: forall a (f :: a -> Type) (xs :: [[[[a]]]]). (HasDict1 a)
  => Sing xs
  -> SOP (SOP f) xs
  -> SOP f (Map ("Concat" :# '()) (Concat (Map ("Distribute" :# '()) xs)))
joinSOP sxs =
  SOP .
  concatNP sxs .
  concatNS sxs .
  innerToNSNP sxs .
  innerDistributePOS sxs .
  innerToPOS sxs

-- | Un-join nested `SOP`'s
--
-- @
--  \sxs ->
--  `innerFromPOS` sxs .
--  `innerUnDistributePOS` sxs .
--  `innerFromNSNP` sxs .
--  `unConcatNS` sxs .
--  `unConcatNP` sxs .
--  `SOP.unSOP`
-- @
unJoinSOP :: forall a (f :: a -> Type) (xs :: [[[[a]]]]). (HasDict1 a)
  => Sing xs
  -> SOP f (Map ("Concat" :# '()) (Concat (Map ("Distribute" :# '()) xs)))
  -> SOP (SOP f) xs
unJoinSOP sxs =
  innerFromPOS sxs .
  innerUnDistributePOS sxs .
  innerFromNSNP sxs .
  unConcatNS sxs .
  unConcatNP sxs .
  SOP.unSOP

