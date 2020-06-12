{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE NoTemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.EntryPoints.Sing.Alg where

import Control.Monad
import Data.Kind
import Data.Functor.Classes
import Prelude hiding (unwords, unlines, show, set, fail)
import GHC.Generics ((:.:)(..))
import Text.Show

import Lorentz (Value)

import Michelson.Typed.Annotation.Path

import Michelson.Typed.T.Alg
import Michelson.Typed.Value.Free
import Michelson.Typed.EntryPoints.Sing.Alg.Types.TH
import Michelson.Typed.EntryPoints.Sing.Alg.Fields
import Michelson.Typed.EntryPoints.Sing.Alg.Field

import Control.Lens.Setter
import Data.Singletons
import Data.Singletons.Prelude.List
import Data.Constraint

import Control.AltError
import Data.AltError.Run

import Data.Constraint.HasDict1
import Data.Singletons.WrappedSing

import Data.SOP (I(..), K(..), NP, NS)
import qualified Data.SOP as SOP

-- after conversion, likely want to expose top-layer NS, currently encoded as NP (f :.: _):
-- (f a -> m (g a)) -> NP f xs -> m (NS g xs)
-- | `EpValue` where the fields may be empty.
newtype EpValueF (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) where
  EpValueF :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t). ()
    => NP (EpFields f t ann) (EpPaths ann)
    -> EpValueF f t ann

deriving instance (SOP.All (SOP.Compose Show (EpFields f t ann)) (EpPaths ann)) => Show (EpValueF f t ann)

-- | An empty `EpValueF`, using `emptyEpFields`
emptyEpValueF :: forall f (t :: TAlg) (ann :: SymAnn t). AltError [String] f
  => Sing t
  -> Sing ann
  -> EpValueF f t ann
emptyEpValueF st sann =
  withDict (singAllSingI $ sEpPaths sann) $
  EpValueF $
  SOP.hcpure (Proxy @SingI) $ emptyEpFields st sann sing

-- | Transform the @f@ in `EpValueF`
transEpValueF :: forall (f :: Type -> Type) (g :: Type -> Type) (t :: TAlg) (ann :: SymAnn t). ()
  => (forall t'. SingI t' => f (ValueOpq t') -> g (ValueOpq t'))
  -> Sing t
  -> Sing ann
  -> EpValueF f t ann
  -> EpValueF g t ann
transEpValueF trans' st sann (EpValueF xs) =
  withDict (singAllSingI $ sEpPaths sann) $
  EpValueF $
  SOP.hmap (transEpFields trans' st sann) xs

lensEpValueF :: forall f (t :: TAlg) (ann :: SymAnn t). (Show1 f, AltError [String] f)
  => Sing t
  -> Sing ann
  -> Lens' (ValueAlgT f t) (EpValueF f t ann)
lensEpValueF st sann fs xs =
  withDict (singAllSingI $ sEpPaths sann) $
  (\(EpValueF ys) -> flip appEndo xs $
    withDict (singAllSingI $ sEpPaths sann) $
    SOP.hcfoldMap
      (Proxy @SingI)
      (\zs@(EpFields sepPath _) -> Endo $
        lensEpFields st sann sepPath `set` zs
      )
      ys
  ) <$>
  fs (EpValueF $
    SOP.hcmap
      (Proxy @SingI)
      (\(WrapSing sfieldName) ->
        lensEpFields st sann sfieldName `view` xs
      ) $
    npWrappedSing $
    sEpPaths sann
  )

-- | A proof that `SingI` implies `Show` implies `SOP.All` `Show`
prfAllShow :: forall a g (xs :: [a]). (HasDict1 a, forall x. SingI x => Show (g x)) => Sing xs -> Dict (SOP.All (SOP.Compose Show g) xs)
prfAllShow SNil = Dict
prfAllShow (SCons sx sxs) =
  withDict (prfAllShow @a @g sxs) $
  withDict1 sx $
  Dict

-- | Convert `NP` to `NS` by picking the only non-erroring
-- entrypoint as the sum choice
npToNS :: forall a (f :: Type -> Type) (g :: a -> Type) (xs :: [a]). (HasDict1 a, AltError [String] f, forall x. SingI x => Show (g x))
  => Sing xs
  -> NP (f :.: g) xs
  -> f (NS g xs)
npToNS SNil SOP.Nil = altErr ["npToNS SOP.Nil"]
npToNS (SCons sx sxs) ((SOP.:*) (Comp1 xs) xss) =
  withDict1 sx $
  withDict (prfAllShow @_ @g sxs) $
  fmap SOP.Z xs <||>
  fmap SOP.S (npToNS sxs xss)

-- | Use `altErr` to make an empty `NP`
emptyNP :: forall a (f :: Type -> Type) (g :: a -> Type) (xs :: [a]). (HasDict1 a, SingKind a, Show (Demote a), AltError [String] f, forall x. SingI x => Show (g x), SingI xs, SOP.SListI xs)
  => NP (f :.: g) xs
emptyNP =
  case SOP.sList @xs of
    SOP.SNil -> SOP.Nil
    SOP.SCons ->
      case sing @xs of
        SCons sx sxs -> withDict1 sx $ withDict1 sxs $
          Comp1 (altErr . ("emptyNP SOP.SCons: " :) . (:[]) . show . fromSing $ sing @xs) SOP.:* emptyNP

-- | Convert `NS` to `NP` by filling all
-- non-chosen slots with `altErr` and running
-- `pure` on the single chosen slot
nsToNP :: forall a (f :: Type -> Type) (g :: a -> Type) (xs :: [a]). (HasDict1 a, SingKind a, Show (Demote a), AltError [String] f, forall x. Show x => Show (f x), forall x. SingI x => Show (g x), SingI xs, SOP.SListI xs)
  => NS g xs
  -> NP (f :.: g) xs
nsToNP (SOP.Z xs) =
  case sing @xs of
    SCons _sx sxs -> withDict1 sxs $
      withDict (prfAllShow @_ @g $ sing @xs) $
      withDict (prfAllShow @_ @(f :.: g) $ sing @xs) $
      Comp1 (pure xs)
      SOP.:* emptyNP
nsToNP (SOP.S xs) =
  case sing @xs of
    SCons _ sxs -> withDict1 sxs $
      withDict (prfAllShow @_ @g $ sing @xs) $
      withDict (prfAllShow @_ @(f :.: g) $ sing @xs) $
      Comp1 (altErr . ("nsToNP SCons: " :) . (:[]) . show . fromSing $ sing @xs)
      SOP.:* nsToNP xs

-- |
newtype EpValue (t :: TAlg) (ann :: SymAnn t) where
  EpValue :: forall (t :: TAlg) (ann :: SymAnn t). ()
    => NS (EpFields I t ann) (EpPaths ann)
    -> EpValue t ann

deriving instance (SOP.All (SOP.Compose Show (EpFields I t ann)) (EpPaths ann)) => Show (EpValue t ann)

-- | Convert an `EpValue` to the `Value` it represents
runEpValue :: forall f (t :: TAlg) (ann :: SymAnn t). (AltError [String] f, Monad f, Show1 f, forall x. Show x => Show (f x))
  => Sing t
  -> Sing ann
  -> EpValue t ann
  -> f (Value (FromTAlg t))
runEpValue st sann xs =
  withDict1 st $
  withDict1 sann $
  withDict (prfAllShow @_ @(EpFields f t ann) (sEpPaths sann)) $
  id $ do
    let ys = toEpValueF st sann xs
    zs <-
      runValueAlgT $
      set (lensEpValueF st sann) ys $ altErrValueAlgT ["runEpValue", show $ fromSing st] st
    return $ fromValueAlg zs

-- | Assert that no fields error and convert to an `EpValue`
fromEpValueF :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t). AltError [String] f
  => Sing t
  -> Sing ann
  -> EpValueF f t ann
  -> f (EpValue t ann)
fromEpValueF st sann (EpValueF xs) =
  withDict (singAllSingI $ sEpPaths sann) $
  withDict1 st $
  withDict1 sann $
  EpValue <$> npToNS (sEpPaths sann) (SOP.hmap (Comp1 . unwrapEpFields st sann) xs)

-- See `prfAllShow`
prfAllShowEpFields :: forall t (ann :: SymAnn t) xs. Sing t -> Sing ann -> Sing xs -> Dict (SOP.All (SOP.Compose Show (EpFields I t ann)) xs)
prfAllShowEpFields _st _sann SNil = Dict
prfAllShowEpFields st sann (SCons _sx sxs) =
  withDict1 st $
  withDict1 sann $
  withDict (prfAllShowEpFields st sann sxs) $
  Dict

-- | Convert an `EpValue` to an `EpValueF`
toEpValueF :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t). (AltError [String] f, Show1 f, forall x. Show x => Show (f x))
  => Sing t
  -> Sing ann
  -> EpValue t ann
  -> EpValueF f t ann
toEpValueF st sann (EpValue xs) =
  withDict (singAllSingI $ sEpPaths sann) $
  withDict1 st $ -- for tests
  withDict1 sann $ -- for tests
  withDict1 (sEpPaths sann) $
  withDict (prfAllShowEpFields st sann $ sing @(EpPaths ann)) $
  EpValueF @f @t @ann $
  SOP.hcmap (Proxy @SingI) (wrapEpFields st sann . unComp1) $
  nsToNP @EpPath @f $
  xs

-- | Extract the fields from an `EpValue`
epValueFields :: forall (t :: TAlg) (ann :: SymAnn t) r. ()
  => (forall (epPath :: EpPath). ()
    => Sing epPath
    -> RunAltE WrappedSing (NP (EpField I t ann epPath)) (EpFieldNamesErrM t ann epPath)
    -> r)
  -> Sing t
  -> Sing ann
  -> EpValue t ann
  -> r
epValueFields f _st sann (EpValue xs) =
  withDict (singAllSingI $ sEpPaths sann) $
  SOP.hcollapse $
  SOP.hmap mapper xs
  where
    mapper :: forall (epPath :: EpPath). ()
      => EpFields I t ann epPath
      -> K r epPath
    mapper (EpFields sepPath xss) = K $ f sepPath xss

