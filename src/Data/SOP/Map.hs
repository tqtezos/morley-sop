{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Data.SOP.Map where

import Data.Kind
import Prelude (($))
import GHC.TypeLits
import Data.Functor.Identity

import Named
import Data.Constraint
import Data.Singletons
import Data.Singletons.Prelude.List (Sing(..))
import Data.SOP (NP(..), All, Prod, HTrans(..))
import Data.SOP.Constraint (AllZipF, SameShapeAs, Top)

import Data.Constraint.HasDict1


-- data instance Sing :: NamedF f a name -> Type where
--   SArgF :: forall (xs :: f a). Sing xs -> Sing ('ArgF xs)

type (:#) (name :: Symbol) (a :: k) = NamedF Identity (Sing a) name


class c a b => Flip c b a where
instance c a b => Flip c b a

singAllTop :: forall a (xs :: [a]). Sing xs -> Dict (All Top xs)
singAllTop SNil = Dict
singAllTop (SCons _ sxs) =
  case singAllTop sxs of
    Dict -> Dict

singAllTop2 :: forall a (xs :: [[a]]). Sing xs -> Dict (All (All Top) xs)
singAllTop2 SNil = Dict
singAllTop2 (SCons sx sxs) =
  case (singAllTop sx, singAllTop2 sxs) of
    (Dict, Dict) -> Dict


-- | The output of `Mapper`
type family MapOut (name :: Type) (x :: a) :: Type

-- | A type family that can be mapped
class (HasDict1 a, HasDict1 (MapOut name a)) => HasMap (name :: Type) (a :: Type) where
  -- | A type family that may be mapped
  type Mapper (name :: Type) (x :: a) :: MapOut name a

  -- | `Mapper` preserves `SingI`
  singIMapper :: forall (x :: a). SingI x :- SingI (Mapper name x)
  singIMapper = Sub $
    evidence1 $
    singMapper @name @a (sing @x)

  -- | `Mapper` preserves `Sing`
  singMapper :: forall (x :: a). Sing x -> Sing (Mapper name x)

-- | Map the `Mapper` of the given name over the list of inputs
type family Map (name :: Type) (xs :: [a]) :: [MapOut name a] where
  Map _    '[]       = '[]
  Map name (x ': xs) = Mapper name x ': Map name xs

-- | A hack, was getting an overlapping instance error in `singIMap`
singEmptyList :: Dict (SingI '[])
singEmptyList = Dict

-- | Proof that `Map` preserves `SingI` (when `Mapper` does)
singIMap :: forall (name :: Type) a (xs :: [a]). HasMap name a => SingI xs :- SingI (Map name xs)
singIMap = Sub $
  case sing @xs of
    SNil -> singEmptyList
    SCons (sx :: Sing x') (sxs :: Sing xs') ->
      withDict (evidence1 sx) $
      withDict (evidence1 sxs) $
      withDict (singIMapper @name @a @x') $
      withDict (singIMap @name @a @xs') $
      Dict

-- | Proof that `Map` preserves `Sing` (when `Mapper` does)
singMap :: forall (name :: Type) a (xs :: [a]). HasMap name a => Sing xs -> Sing (Map name xs)
singMap SNil = SNil
singMap (SCons (sx :: Sing x') (sxs :: Sing xs')) =
  mappedX `SCons` mappedXs
  where
    mappedX :: Sing (Mapper name x')
    mappedX = singMapper @name @a @x' sx

    mappedXs :: Sing (Map name xs')
    mappedXs = singMap @name @a sxs

-- | `Map` preserves shape
sameShapeMap :: forall name a (xs :: [a]). HasMap name a
  => Sing xs
  -> Dict (SameShapeAs xs (Map name xs), SameShapeAs (Map name xs) xs)
sameShapeMap SNil = Dict
sameShapeMap (SCons _ sxs) =
  case sameShapeMap @name sxs of
    Dict -> Dict

-- | Relational form of `Mapper` for `AllZipF`
class (SingI x, Mapper name x ~ y) => IsMapper (name :: Type) (x :: a) (y :: MapOut name a) where
instance (SingI x, Mapper name x ~ y) => IsMapper name x y where

-- | Proof that zipping @xs@ and @`Map` name xs@ with (`IsMapper` name) holds
zipMapper ::
     forall name a (xs :: [a]). HasMap name a
  => Sing xs
  -> Dict ( All Top xs
          , All Top (Map name xs)
          , SameShapeAs xs (Map name xs)
          , SameShapeAs (Map name xs) xs
          , AllZipF (IsMapper name) xs (Map name xs)
          , AllZipF (Flip (IsMapper name)) (Map name xs) xs)
zipMapper SNil = Dict
zipMapper (SCons sx sxs) =
  withDict (evidence1 sx) $
  withDict (singAllTop sxs) $
  withDict (singAllTop (singMap @name sxs)) $
  withDict (sameShapeMap @name sxs) $
  case zipMapper @name sxs of
    Dict -> Dict

-- | Map the indices of some `HTrans` pair
ixmap :: forall name a (f :: a -> Type) (g :: MapOut name a -> Type) h1 h2 (xs :: [a]). (HasMap name a, HTrans h1 h2, Prod h1 ~ NP)
  => (forall (x :: a). Sing x -> f x -> g (Mapper name x))
  -> Sing xs
  -> h1 f xs
  -> h2 g (Map name xs)
ixmap fs sxs fxs =
  withDict (zipMapper @name @a sxs) $
  htrans (Proxy @(IsMapper name)) (\x -> fs sing x) fxs

-- | Unmap the indices of some `HTrans` pair
ixunmap :: forall name a (f :: a -> Type) (g :: MapOut name a -> Type) h1 h2 (xs :: [a]). (HasMap name a, HTrans h1 h2, Prod h1 ~ NP)
  => (forall (x :: a). Sing x -> g (Mapper name x) -> f x)
  -> Sing xs
  -> h1 g (Map name xs)
  -> h2 f xs
ixunmap fs sxs fxs =
  withDict (zipMapper @name @a sxs) $
  htrans (Proxy @(Flip (IsMapper name))) (\x -> fs' x) fxs
  where
    fs' :: forall (y :: a). SingI y => g (Mapper name y) -> f y
    fs' = fs sing

