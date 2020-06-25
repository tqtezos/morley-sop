{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS -Wno-orphans -Wno-missing-export-lists #-}

module Michelson.Typed.EntryPoints.Sing.Alg.Field where

import Data.Kind
import Data.Functor.Classes
import Prelude hiding (All, unwords, show)
import GHC.TypeLits (Symbol)
import GHC.Generics ((:.:)(..))
import Text.Show

import Control.AltError
import Data.AltError
import Data.AltError.TH
import Data.AltError.Run

import Michelson.Typed.Annotation.Path

import Michelson.Typed.T.Alg
import Michelson.Typed.Value.Free
import Michelson.Typed.EntryPoints.Sing.Alg.Types.TH
import Michelson.Typed.EntryPoints.Sing.Alg.Lens
import Data.Constraint.HasDict1
import Data.Singletons.WrappedSing

import Data.SOP (I(..), All, NP)
import qualified Data.SOP as SOP
import Data.Singletons
import Data.Singletons.Prelude.List hiding (All)
import Data.Constraint

-- | `Sing` implies `All` `SingI`
singAllSingI :: forall a (xs :: [a]). HasDict1 a
  => Sing xs
  -> Dict (All SingI xs)
singAllSingI SNil = Dict
singAllSingI (SCons sx sxs) =
  withDict1 sx $
  case singAllSingI sxs of
    Dict -> Dict

-- | Produce a `NP` from a `Sing` list by exposing the `Sing` elements
-- with `WrappedSing`
npWrappedSing :: forall a (xs :: [a]). HasDict1 a
  => Sing xs
  -> NP WrappedSing xs
npWrappedSing sxs =
  withDict (singAllSingI sxs) $
  SOP.hcpure (Proxy @SingI) wrapSing
  where
    wrapSing :: forall x. SingI x => WrappedSing x
    wrapSing = WrapSing sing

-- | A single field of a value
data EpField (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Maybe Symbol) where
  EpField :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Maybe Symbol). ()
    => Sing fieldName
    -> RunSingValueOpq f (EpFieldT t ann epPath fieldName)
    -> EpField f t ann epPath fieldName

instance (forall t'. SingI t' => Show (f (ValueOpq t')), SingI t, SingI ann, SingI epPath) => Show (EpField f t ann epPath fieldName) where
  showsPrec d (EpField sfieldName xs) =
    withDict1 (sEpFieldT @Symbol @ErrM (sing @t) (sing @ann) (sing @epPath) sfieldName) $
    showsBinaryWith showsPrec showsPrec "EpField" d (fromSing sfieldName) xs

-- | Expose the effects of @f@ at the top level of an `EpField`
unwrapEpField :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Maybe Symbol). Applicative f
  => EpField f t ann epPath fieldName
  -> f (EpField I t ann epPath fieldName)
unwrapEpField (EpField sfieldName xs) =
  EpField sfieldName <$>
  case xs of
    RunAltThrow ys -> pure $ RunAltThrow ys
    RunAltExcept ys -> pure $ RunAltExcept ys
    RunPureAltE (Comp1 ys) -> RunPureAltE . Comp1 . I <$> ys

-- | Embed the effects of @f@ at the top level of an `EpField`
wrapEpField :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Maybe Symbol). (Functor f, SingI epPath, SingI fieldName)
  => Sing t
  -> Sing ann
  -> f (EpField I t ann epPath fieldName)
  -> EpField f t ann epPath fieldName
wrapEpField st sann xs =
  EpField (sing @fieldName) $
  case sEpFieldT @Symbol @ErrM st sann (sing @epPath) (sing @fieldName) of
    SAltThrow serr -> RunAltThrow $ WrapSing serr
    SAltExcept serr -> RunAltExcept $ WrapSing serr
    SPureAltE _sResult -> RunPureAltE $ Comp1 $
      (\case
         EpField _sfieldName xss -> SOP.unI $ unComp1 $ unRunPureAltE xss
      ) <$> xs

-- | Make an empty `EpField` using `altErr`
emptyEpField :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Maybe Symbol). AltError [String] f
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> Sing fieldName
  -> EpField f t ann epPath fieldName
emptyEpField st sann sepPath sfieldName =
  EpField sfieldName $
  case sEpFieldT @Symbol @ErrM st sann sepPath sfieldName of
    SAltThrow serr -> RunAltThrow $ WrapSing serr
    SAltExcept serr -> RunAltExcept $ WrapSing serr
    SPureAltE sResult -> RunPureAltE $ Comp1 . altErr . ("emptyEpField SPureAltE: " :) . (: []) . show $ fromSing sResult

-- | Transform the @f@ of an `EpField`
transEpField :: forall (f :: Type -> Type) (g :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Maybe Symbol). ()
  => (forall t'. SingI t' => f (ValueOpq t') -> g (ValueOpq t'))
  -> Sing t
  -> Sing ann
  -> Sing epPath
  -> EpField f t ann epPath fieldName
  -> EpField g t ann epPath fieldName
transEpField trans' st sann sepPath (EpField sfieldName xs) =
  EpField sfieldName $
  case (sEpFieldT @Symbol @ErrM st sann sepPath sfieldName, xs) of
    (SAltThrow _, RunAltThrow xss) -> RunAltThrow xss
    (SAltExcept _, RunAltExcept xss) -> RunAltExcept xss
    (SPureAltE st', RunPureAltE (Comp1 xss)) -> withDict1 st' $ RunPureAltE $ Comp1 $ trans' xss

