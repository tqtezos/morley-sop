{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists -Wall -Wno-orphans #-}

module Michelson.Typed.T.Sing where

import Prelude (($), error)
import Data.Maybe
import Data.String
import Data.Typeable

import Michelson.Typed.Sing
import Michelson.Typed.Scope
import Michelson.Typed.T

import Data.Constraint
import Data.Singletons.Prelude
import Data.Singletons.TH

import Data.Constraint.HasDict1


instance HasDict1 CT where
  evidence1 = $(gen_evidence1 ''CT)

instance HasDict1 T where
  evidence1 = $(gen_evidence1 ''T)

-- | Proof that `Sing` implies `Typeable`
singTypeableCT :: forall (t :: CT). Sing t -> Dict (Typeable t)
singTypeableCT x = $(sCases ''CT [|x|] [|Dict|])

-- | Proof that `Sing` implies `Typeable`
singTypeableT :: forall (t :: T). Sing t -> Dict (Typeable t)
singTypeableT (STc ct) =
  withDict (singTypeableCT ct) $
  Dict
singTypeableT STKey = Dict
singTypeableT STUnit = Dict
singTypeableT STSignature = Dict
singTypeableT STChainId = Dict
singTypeableT (STOption st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STList st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STSet st) =
  withDict (singTypeableCT st) $
  Dict
singTypeableT STOperation  = Dict
singTypeableT (STContract st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STPair st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STOr st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STLambda st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STMap st su) =
  withDict (singTypeableCT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STBigMap st su) =
  withDict (singTypeableCT st) $
  withDict (singTypeableT su) $
  Dict


$(genDefunSymbols [''CT, ''T])
$(singShowInstances [''CT, ''T])

-- | Assert `HasNoOp` or fail with an `error`
assertOpAbsense :: forall (t :: T) a. Sing t -> (HasNoOp t => a) -> a
assertOpAbsense st f =
  case opAbsense st of
    Nothing -> error "assertOpAbsense"
    Just Dict -> withDict1 st $ forbiddenOp @t f

-- | Assert `HasNoNestedBigMaps` or fail with an `error`
assertNestedBigMapsAbsense :: forall (t :: T) a. Sing t -> (HasNoNestedBigMaps t => a) -> a
assertNestedBigMapsAbsense st f =
  case nestedBigMapsAbsense st of
    Nothing -> error "assertNestedBigMapsAbsense"
    Just Dict -> withDict1 st $ forbiddenNestedBigMaps @t f

