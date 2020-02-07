{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}

module Data.SOP.Deep where

import Data.Kind
import Prelude (($), (.))

import Data.Constraint
import Data.Singletons
import Data.Singletons.Prelude.List (Sing(..))
import Data.SOP (SOP(..), I(..), Prod, AllZipN)
import Data.SOP.Constraint (LiftedCoercible)
import qualified Data.SOP as SOP
import Generics.SOP (Generic(..))

import Data.Constraint.HasDict1
import Data.SOP.Map
import Data.SOP.Join

import Data.List.Concat


-- | Expand a single index to its deep representation
type family StepDeep (x :: a) :: [[a]] where
  StepDeep x = StepDeep1 (DCode x)

-- | `StepDeep` preserves `SingI`
singIStepDeep :: forall a f (x :: a). HasDeep a f => SingI x :- SingI (StepDeep x)
singIStepDeep = Sub $
  evidence1 $ singStepDeep (sing @x)

-- | `StepDeep` preserves `Sing`
singStepDeep :: forall a f (x :: a). HasDeep a f => Sing x -> Sing (StepDeep x)
singStepDeep sx =
  singStepDeep1 $ singDCode sx

type instance MapOut ("StepDeep" :# '()) a = [[a]]

instance HasDeep a f => HasMap ("StepDeep" :# '()) a where
  type Mapper ("StepDeep" :# '()) x = StepDeep x
  singIMapper = singIStepDeep @a
  singMapper = singStepDeep @a

type instance MapOut ("MapStepDeep" :# '()) [a] = [[[a]]]

instance HasDeep a f => HasMap ("MapStepDeep" :# '()) [a] where
  type Mapper ("MapStepDeep" :# '()) xs = Map ("StepDeep" :# '()) xs
  singIMapper = singIMap @("StepDeep" :# '()) @a
  singMapper = singMap @("StepDeep" :# '()) @a

-- | Singletons of singletons are shallow, everything else is deep
type family StepDeep1 (code :: [[a]]) :: [[a]] where
  StepDeep1 ((x ': '[]) ': '[]) = (x ': '[]) ': '[]
  StepDeep1 code = Deep code

-- | `StepDeep1` preserves `Sing`
singStepDeep1 :: forall a f (code :: [[a]]). HasDeep a f => Sing code -> Sing (StepDeep1 code)
singStepDeep1 sxs =
  case sxs of
    SNil ->
      singDeep sxs
    SCons SNil _ ->
      singDeep sxs
    SCons (SCons _ SNil) SNil ->
      sxs
    SCons (SCons _ SNil) (SCons _ _) ->
      singDeep sxs
    SCons (SCons _ (SCons _ _)) _ ->
      singDeep sxs


-- | We expand deeply by `MapStepDeep1`ing and joining the results
type family Deep (xs :: [[a]]) :: [[a]] where
  Deep xs = Map ("Concat" :# '()) (Concat (Map ("Distribute" :# '()) (Map ("MapStepDeep" :# '()) xs)))

-- | `Deep` preserves `Sing`
singDeep :: forall a f (xs :: [[a]]). HasDeep a f => Sing xs -> Sing (Deep xs)
singDeep =
  singMap @("Concat" :# '()) .
  singConcat .
  singMap @("Distribute" :# '()) .
  singMap @("MapStepDeep" :# '())


-- | An indexed form of `Generic` that allows deep conversion to/from `SOP`
--
-- @
-- -- | Unsafe un-application
-- type family UnF (f :: a -> Type) (y :: Type) :: a where
--   UnF f (f x) = x
--   UnF f x = TypeError ('Text "UnF expected (" ':<>: 'ShowType f ':<>: 'Text " _) but got " ':<>: 'ShowType x)
--
-- MapUnF
-- MapUnF1
--
-- -- | If your @f@ is consistently recognized by `UnF`,
-- -- `GDCode` should be equivalent to `DCode`
-- type family GDCode (f :: a -> Type) (x :: a) where
--   GDCode f x = MapUnF1 f (Code (f x))
-- @
class HasDict1 a => HasDeep a (f :: a -> Type) | a -> f where
  -- | A Dependent/Deep `Code`
  type DCode (x :: a) :: [[a]]

  -- | `DCode` must preserve `SingI`
  singIDCode :: forall (x :: a). SingI x :- SingI (DCode x)
  singIDCode = Sub $
    evidence1 $
    singDCode $
    sing @x

  -- | `DCode` must preserve `Sing`
  singDCode :: forall (x :: a). Sing x -> Sing (DCode x)
  singDCode sx =
    withDict
      (singIDCode `mapDict` evidence1 sx)
      sing

  -- | `to` for `DCode`
  toD :: forall (x :: a). Sing x -> SOP f (DCode x) -> f x
  default toD :: forall (x :: a). (Generic (f x), AllZipN (Prod SOP) (LiftedCoercible f I) (DCode x) (Code (f x))) => Sing x -> SOP f (DCode x) -> f x
  toD sx sop =
    withDict (singAllTop (singDCode sx)) $
    to $ SOP.htoI sop

  -- | `from` for `DCode`
  fromD :: forall (x :: a). Sing x -> f x -> SOP f (DCode x)
  default fromD :: forall (x :: a). (Generic (f x), AllZipN (Prod SOP) (LiftedCoercible I f) (Code (f x)) (DCode x)) => Sing x -> f x -> SOP f (DCode x)
  fromD sx fx =
    withDict (singAllTop (singDCode sx)) $
    SOP.hfromI $ from fx


-- | Convert an embedded value to a `SOP` of `StepDeep`
-- by conditionally applying `fromD` or `fromDeep` depending on the
-- result of `singDCode`
fromStepDeep :: forall a f (x :: a). HasDeep a f => Sing x -> f x -> SOP f (StepDeep x)
fromStepDeep sx fx =
  case singDCode sx of
    SNil ->
      fromD sx fx
    SCons sy sys ->
      case sys of
        SNil ->
          case sy of
            SNil ->
              fromDeep sx fx
            SCons _ szs ->
              case szs of
                SNil ->
                  fromD sx fx
                SCons _ _ ->
                  fromDeep sx fx
        SCons _ _ ->
          fromDeep sx fx

-- | Map `fromStepDeep` over a `SOP` of `DCode` for some value
fromMapStepDeep1 :: forall a f (x :: a). HasDeep a f => Sing x -> SOP f (DCode x) -> SOP (SOP f) (Map ("MapStepDeep" :# '()) (DCode x))
fromMapStepDeep1 sx (SOP sopDCode) =
  SOP $
  ixmap @("MapStepDeep" :# '()) @[a]
    (ixmap @("StepDeep" :# '()) @a
      (fromStepDeep @a @f)
    )
    (singDCode @a @f sx)
    sopDCode

-- | Convert an embedded value to a `Deep` `SOP` of its `DCode`
fromDeep :: forall a f (x :: a). HasDeep a f => Sing x -> f x -> SOP f (Deep (DCode x))
fromDeep sx fx =
  joinSOP (singMap @("MapStepDeep" :# '()) (singDCode sx)) $
  fromMapStepDeep1 sx $
  fromD sx fx


-- | Convert a `SOP` of `StepDeep` to an embedded value
-- by conditionally applying `fromD` or `fromDeep` depending on the
-- result of `singDCode`
toStepDeep :: forall a f (x :: a). HasDeep a f => Sing x -> SOP f (StepDeep x) -> f x
toStepDeep sx sop =
  case singDCode sx of
    SNil ->
      toD sx sop
    SCons sy sys ->
      case sys of
        SNil ->
          case sy of
            SNil ->
              toDeep sx sop
            SCons _ szs ->
              case szs of
                SNil ->
                  toD sx sop
                SCons _ _ ->
                  toDeep sx sop
        SCons _ _ ->
          toDeep sx sop

-- | Map `toStepDeep` to a `SOP` of `DCode` for some value
toMapStepDeep1 :: forall a f (x :: a). HasDeep a f => Sing x -> SOP (SOP f) (Map ("MapStepDeep" :# '()) (DCode x)) -> SOP f (DCode x)
toMapStepDeep1 sx (SOP sopsDCode) =
  SOP $
  ixunmap @("MapStepDeep" :# '()) @[a]
    (ixunmap @("StepDeep" :# '()) @a
      (toStepDeep @a @f)
    )
    (singDCode @a @f sx)
    sopsDCode

-- | Convert a `Deep` `SOP` of a value's `DCode` to an embedding of it
toDeep :: forall a f (x :: a). HasDeep a f => Sing x -> SOP f (Deep (DCode x)) -> f x
toDeep sx sop =
  toD sx $
  toMapStepDeep1 sx $
  unJoinSOP (singMap @("MapStepDeep" :# '()) (singDCode sx)) $
  sop

