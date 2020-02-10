{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}

module Data.SOP.Deep.Annotated where

import Data.Kind
-- import Data.List.NonEmpty (NonEmpty(..))
import GHC.TypeLits
import Prelude (($), NonEmpty(..), fromString, error)

import Data.Constraint
import Data.Singletons
import Data.Singletons.Prelude.List (Sing(..))
-- import Data.SOP (SOP(..), I(..), Prod, AllZipN)
-- import Data.SOP.Constraint (LiftedCoercible)
-- import qualified Data.SOP as SOP
-- import Generics.SOP (Generic(..))

import Data.Constraint.HasDict1
-- import Data.SOP.Map
-- import Data.SOP.Join

-- import Data.List.Concat

import Data.SOP.Deep

-- data AnnotatedList (xs :: [a]) where
--   AnnotatedListNil :: AnnotatedList '[]
--   AnnotatedListCons :: ann -> x -> AnnotatedList xs -> AnnotatedList (x ': xs)

data Annotated (str :: Type) (a :: Type) where
  Annotate :: [[NonEmpty str]] -> a -> Annotated str a

data instance Sing :: Annotated str a -> Type where
  SAnnotate :: Sing xs -> Sing x -> Sing ('Annotate xs x)

instance (SingI xs, SingI x) => SingI ('Annotate xs x) where sing = SAnnotate sing sing

instance (SingKind str, SingKind a) => SingKind (Annotated str a) where
  type Demote (Annotated str a) = Annotated (Demote str) (Demote a)
  fromSing (SAnnotate sann sx) = Annotate (fromSing sann) (fromSing sx)
  toSing (Annotate ann x) =
    case (toSing ann, toSing x) of
      (SomeSing sann, SomeSing sx) ->
        SomeSing $
        SAnnotate sann sx

instance (HasDict1 str, HasDict1 a) => HasDict1 (Annotated str a) where
  evidence1 (SAnnotate sxs sx) =
    withDict (evidence1 sxs) $
    withDict (evidence1 sx) $
    Dict

data AnnotatedF (f :: a -> Type) (xs :: Annotated str a) where
  AnnotatedF :: Sing ann -> f x -> AnnotatedF f ('Annotate ann x)

type family AnnotateList2 (anns :: [[NonEmpty str]]) (xs :: [[a]]) :: [[Annotated str a]] where
  AnnotateList2 '[] '[] = '[]
  AnnotateList2 (x ': xs) (y ': ys) = AnnotateList x y ': AnnotateList2 xs ys
  AnnotateList2 xs ys = TypeError ('Text "Expected same shape, but found: " ':<>: 'ShowType xs ':<>: 'Text " " ':<>: 'ShowType ys)

-- | Warning: will fail unless lists are of same dimensions
singAnnotateList2 :: forall str a (anns :: [[NonEmpty str]]) (xs :: [[a]]). Sing anns -> Sing xs -> Sing (AnnotateList2 anns xs)
singAnnotateList2 SNil SNil = SNil
singAnnotateList2 SNil (SCons _ _) = error "singAnnotateList: Expected same shape, but annotations smaller than target list"
singAnnotateList2 (SCons _ _) SNil = error "singAnnotateList: Expected same shape, but annotations bigger than target list"
singAnnotateList2 (SCons sa sas) (SCons sx sxs) = SCons (singAnnotateList @str @a sa sx) (singAnnotateList2 sas sxs)

type family AnnotateList (anns :: [NonEmpty str]) (xs :: [a]) :: [Annotated str a] where
  AnnotateList '[] '[] = '[]
  AnnotateList (x ': xs) (y ': ys) = 'Annotate '[ '[x]] y ': AnnotateList xs ys
  AnnotateList xs ys = TypeError ('Text "Expected same shape, but found: " ':<>: 'ShowType xs ':<>: 'Text " " ':<>: 'ShowType ys)

-- | Warning: will fail unless lists are of same dimensions
singAnnotateList :: forall str a (anns :: [NonEmpty str]) (xs :: [a]). Sing anns -> Sing xs -> Sing (AnnotateList anns xs)
singAnnotateList SNil SNil = SNil
singAnnotateList SNil (SCons _ _) = error "singAnnotateList: Expected same shape, but annotations smaller than target list"
singAnnotateList (SCons _ _) SNil = error "singAnnotateList: Expected same shape, but annotations bigger than target list"
singAnnotateList (SCons sa sas) (SCons sx sxs) = SCons (SAnnotate (SCons (SCons sa SNil) SNil) sx) (singAnnotateList sas sxs)

instance (HasDeep a f, HasDict1 str) => HasDeep (Annotated str a) (AnnotatedF f) where
  type DCode ('Annotate anns x) = AnnotateList2 anns (DCode x)

  singDCode :: forall (x :: Annotated str a). Sing x -> Sing (DCode x)
  singDCode (SAnnotate sann sx) = singAnnotateList2 sann (singDCode sx)

  toD = error "toD unimplemented"
  fromD = error "fromD unimplemented"



---- | An indexed form of `Generic` that allows deep conversion to/from `SOP`
----
---- @
---- -- | Unsafe un-application
---- type family UnF (f :: a -> Type) (y :: Type) :: a where
----   UnF f (f x) = x
----   UnF f x = TypeError ('Text "UnF expected (" ':<>: 'ShowType f ':<>: 'Text " _) but got " ':<>: 'ShowType x)
----
---- MapUnF
---- MapUnF1
----
---- -- | If your @f@ is consistently recognized by `UnF`,
---- -- `GDCode` should be equivalent to `DCode`
---- type family GDCode (f :: a -> Type) (x :: a) where
----   GDCode f x = MapUnF1 f (Code (f x))
---- @
--class HasDict1 a => HasDeep a (f :: a -> Type) | a -> f where
--  -- | A Dependent/Deep `Code`
--  type DCode (x :: a) :: [[a]]
--
--  -- | `DCode` must preserve `SingI`
--  singIDCode :: forall (x :: a). SingI x :- SingI (DCode x)
--  singIDCode = Sub $
--    evidence1 $
--    singDCode $
--    sing @x
--
--  -- | `DCode` must preserve `Sing`
--  singDCode :: forall (x :: a). Sing x -> Sing (DCode x)
--  singDCode sx =
--    withDict
--      (singIDCode `mapDict` evidence1 sx)
--      sing
--
--  -- | `to` for `DCode`
--  toD :: forall (x :: a). Sing x -> SOP f (DCode x) -> f x
--  default toD :: forall (x :: a). (Generic (f x), AllZipN (Prod SOP) (LiftedCoercible f I) (DCode x) (Code (f x))) => Sing x -> SOP f (DCode x) -> f x
--  toD sx sop =
--    withDict (singAllTop (singDCode sx)) $
--    to $ SOP.htoI sop
--
--  -- | `from` for `DCode`
--  fromD :: forall (x :: a). Sing x -> f x -> SOP f (DCode x)
--  default fromD :: forall (x :: a). (Generic (f x), AllZipN (Prod SOP) (LiftedCoercible I f) (Code (f x)) (DCode x)) => Sing x -> f x -> SOP f (DCode x)
--  fromD sx fx =
--    withDict (singAllTop (singDCode sx)) $
--    SOP.hfromI $ from fx




