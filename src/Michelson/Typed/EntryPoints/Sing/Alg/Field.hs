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

import Data.Either.Run
import Data.Either.Run.ErrorMessage

import Michelson.Typed.Annotation.Path

import Michelson.Typed.T.Alg
import Michelson.Typed.Value.Free
import Michelson.Typed.EntryPoints.Sing.Alg.Types
import Data.Constraint.HasDict1
import Data.Singletons.WrappedSing

import Data.SOP (I(..), All, NP)
import qualified Data.SOP as SOP
import Data.Singletons
import Data.Singletons.Prelude.List hiding (All)
import Data.Singletons.Prelude.Either
import Data.Constraint


singAllSingI :: forall a (xs :: [a]). HasDict1 a
  => Sing xs
  -> Dict (All SingI xs)
singAllSingI SNil = Dict
singAllSingI (SCons sx sxs) =
  withDict1 sx $
  case singAllSingI sxs of
    Dict -> Dict


npWrappedSing :: forall a (xs :: [a]). HasDict1 a
  => Sing xs
  -> NP WrappedSing xs
npWrappedSing sxs =
  withDict (singAllSingI sxs) $
  SOP.hcpure (Proxy @SingI) wrapSing
  where
    wrapSing :: forall x. SingI x => WrappedSing x
    wrapSing = WrapSing sing

data EpField (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Symbol) where
  EpField :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Symbol). ()
    => Sing fieldName
    -> RunEither SingError (f :.: ValueOpq) (EpFieldT t ann epPath fieldName)
    -> EpField f t ann epPath fieldName

instance (forall t'. SingI t' => Show (f (ValueOpq t')), SingI t, SingI ann, SingI epPath) => Show (EpField f t ann epPath fieldName) where
  showsPrec d (EpField sfieldName xs) =
    withDict1 (sEpFieldT (sing @t) (sing @ann) (sing @epPath) sfieldName) $
    showsBinaryWith showsPrec showsPrec "EpField" d (fromSing sfieldName) xs

-- transEpField :: forall (f :: Type -> Type) (g :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Symbol). ()
--   => (forall x. f x -> g x)
--   -> EpField f t ann epPath fieldName
--   -> EpField g t ann epPath fieldName
-- transEpField trans = _

unwrapEpField :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Symbol). Applicative f
  => EpField f t ann epPath fieldName
  -> f (EpField I t ann epPath fieldName)
unwrapEpField (EpField sfieldName xs) =
  EpField sfieldName <$>
  case xs of
    RunLeft ys -> pure $ RunLeft ys
    RunRight (Comp1 ys) -> RunRight . Comp1 . I <$> ys

-- wrapEpField :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Symbol). Applicative f
--   => EpField I t ann epPath fieldName
--   -> EpField f t ann epPath fieldName
-- wrapEpField (EpField sfieldName xs) =
--   EpField sfieldName $
--   case xs of
--     RunLeft ys -> RunLeft ys
--     RunRight (Comp1 (I ys)) -> RunRight . Comp1 $ pure ys

wrapEpField' :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Symbol). (Functor f, SingI epPath, SingI fieldName)
  => Sing t
  -> Sing ann
  -> f (EpField I t ann epPath fieldName)
  -> EpField f t ann epPath fieldName
wrapEpField' st sann xs =
  EpField (sing @fieldName) $
  case sEpFieldT st sann (sing @epPath) (sing @fieldName) of
    SLeft serr -> RunLeft $ SingError serr
    SRight _sResult -> RunRight $ Comp1 $
      (\case
         EpField _sfieldName xss -> SOP.unI $ unComp1 $ unRunRight xss
      ) <$> xs

emptyEpField :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Symbol). MonadFail f
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> Sing fieldName
  -> EpField f t ann epPath fieldName
emptyEpField st sann sepPath sfieldName =
  EpField sfieldName $
  case sEpFieldT st sann sepPath sfieldName of
    SLeft serr -> RunLeft $ SingError serr
    SRight sResult -> RunRight $ Comp1 . fail . ("emptyEpField SRight: " ++) . show $ fromSing sResult


transEpField :: forall (f :: Type -> Type) (g :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (fieldName :: Symbol). ()
  => (forall t'. SingI t' => f (ValueOpq t') -> g (ValueOpq t'))
  -> Sing t
  -> Sing ann
  -> Sing epPath
  -> EpField f t ann epPath fieldName
  -> EpField g t ann epPath fieldName
transEpField trans' st sann sepPath (EpField sfieldName xs) =
  EpField sfieldName $
  case (sEpFieldT st sann sepPath sfieldName, xs) of
    (SLeft _, RunLeft xss) -> RunLeft xss
    (SRight st', RunRight (Comp1 xss)) -> withDict1 st' $ RunRight $ Comp1 $ trans' xss

