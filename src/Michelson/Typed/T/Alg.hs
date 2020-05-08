{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.T.Alg where

import Data.Eq
import GHC.Generics
import Text.Show
import Data.Function

import Michelson.Typed.T (CT, T)
import qualified Michelson.Typed.T as Michelson
import qualified Michelson.Typed.Sing as Michelson

import Data.Singletons
import Data.Singletons.TH
import Data.Constraint
import Control.DeepSeq

import Michelson.Typed.T.Sing (singICT, singIT, singTypeableCT, singTypeableT)
import Data.Constraint.HasDict1


-- | Opaque `T`
data TOpq
  = TKey
  | TUnit
  | TSignature
  | TChainId
  | TOperation

  | Tc CT
  | TSet CT

  | TOption T
  | TList T
  | TContract T

  | TLambda T T

  | TMap CT T
  | TBigMap CT T
  deriving stock (Eq, Show, Generic)

instance NFData TOpq

$(genSingletons [''TOpq])
$(singShowInstance ''TOpq)

-- | `HasDict1` for `T`
singITOpq :: forall (t :: TOpq). Sing t -> Dict (SingI t)
singITOpq (STc ct) =
  withDict (singICT ct) $
  Dict
singITOpq STKey = Dict
singITOpq STUnit = Dict
singITOpq STSignature = Dict
singITOpq STChainId = Dict
singITOpq (STOption st) =
  withDict (singIT st) $
  Dict
singITOpq (STList st) =
  withDict (singIT st) $
  Dict
singITOpq (STSet st) =
  withDict (singICT st) $
  Dict
singITOpq STOperation  = Dict
singITOpq (STContract st) =
  withDict (singIT st) $
  Dict
singITOpq (STLambda st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singITOpq (STMap st su) =
  withDict (singICT st) $
  withDict (singIT su) $
  Dict
singITOpq (STBigMap st su) =
  withDict (singICT st) $
  withDict (singIT su) $
  Dict

instance HasDict1 TOpq where
  evidence1 = singITOpq

-- | Algebraic `T`
data TAlg
  = TPair TAlg TAlg
  | TOr TAlg TAlg
  | TOpq TOpq
  deriving stock (Eq, Show, Generic)

instance NFData TAlg

$(genSingletons [''TAlg])
$(singShowInstance ''TAlg)

singITAlg :: forall (t :: TAlg). Sing t -> Dict (SingI t)
singITAlg (STPair st su) =
  withDict (singITAlg st) $
  withDict (singITAlg su) $
  Dict
singITAlg (STOr st su) =
  withDict (singITAlg st) $
  withDict (singITAlg su) $
  Dict
singITAlg (STOpq st) =
  withDict (singITOpq st) $
  Dict

instance HasDict1 TAlg where
  evidence1 = singITAlg

type family ToTAlg (t :: T) :: TAlg where
  ToTAlg ('Michelson.Tc t1) = 'TOpq ('Tc t1)
  ToTAlg ('Michelson.TKey) = 'TOpq ('TKey)
  ToTAlg ('Michelson.TUnit) = 'TOpq ('TUnit)
  ToTAlg ('Michelson.TSignature) = 'TOpq ('TSignature)
  ToTAlg ('Michelson.TChainId) = 'TOpq ('TChainId)
  ToTAlg ('Michelson.TOption t1) = 'TOpq ('TOption t1)
  ToTAlg ('Michelson.TList t1) = 'TOpq ('TList t1)
  ToTAlg ('Michelson.TSet t1) = 'TOpq ('TSet t1)
  ToTAlg ('Michelson.TOperation) = 'TOpq ('TOperation)
  ToTAlg ('Michelson.TContract t1) = 'TOpq ('TContract t1)
  ToTAlg ('Michelson.TLambda t1 t2) = 'TOpq ('TLambda t1 t2)
  ToTAlg ('Michelson.TMap t1 t2) = 'TOpq ('TMap t1 t2)
  ToTAlg ('Michelson.TBigMap t1 t2) = 'TOpq ('TBigMap t1 t2)
  ToTAlg ('Michelson.TPair t1 t2) = 'TPair (ToTAlg t1) (ToTAlg t2)
  ToTAlg ('Michelson.TOr t1 t2) = 'TOr (ToTAlg t1) (ToTAlg t2)

singToTAlg :: Sing t -> Sing (ToTAlg t)
singToTAlg (Michelson.STc t1) = STOpq (STc t1)
singToTAlg (Michelson.STKey) = STOpq (STKey)
singToTAlg (Michelson.STUnit) = STOpq (STUnit)
singToTAlg (Michelson.STSignature) = STOpq (STSignature)
singToTAlg (Michelson.STChainId) = STOpq (STChainId)
singToTAlg (Michelson.STOption t1) = STOpq (STOption t1)
singToTAlg (Michelson.STList t1) = STOpq (STList t1)
singToTAlg (Michelson.STSet t1) = STOpq (STSet t1)
singToTAlg (Michelson.STOperation) = STOpq (STOperation)
singToTAlg (Michelson.STContract t1) = STOpq (STContract t1)
singToTAlg (Michelson.STLambda t1 t2) = STOpq (STLambda t1 t2)
singToTAlg (Michelson.STMap t1 t2) = STOpq (STMap t1 t2)
singToTAlg (Michelson.STBigMap t1 t2) = STOpq (STBigMap t1 t2)
singToTAlg (Michelson.STPair t1 t2) = STPair (singToTAlg t1) (singToTAlg t2)
singToTAlg (Michelson.STOr t1 t2) = STOr (singToTAlg t1) (singToTAlg t2)


type family FromTOpq (t :: TOpq) = (r :: T) | r -> t where
  FromTOpq ('Tc t1) = 'Michelson.Tc t1
  FromTOpq ('TKey) = 'Michelson.TKey
  FromTOpq ('TUnit) = 'Michelson.TUnit
  FromTOpq ('TSignature) = 'Michelson.TSignature
  FromTOpq ('TChainId) = 'Michelson.TChainId
  FromTOpq ('TOption t1) = 'Michelson.TOption t1
  FromTOpq ('TList t1) = 'Michelson.TList t1
  FromTOpq ('TSet t1) = 'Michelson.TSet t1
  FromTOpq ('TOperation) = 'Michelson.TOperation
  FromTOpq ('TContract t1) = 'Michelson.TContract t1
  FromTOpq ('TLambda t1 t2) = 'Michelson.TLambda t1 t2
  FromTOpq ('TMap t1 t2) = 'Michelson.TMap t1 t2
  FromTOpq ('TBigMap t1 t2) = 'Michelson.TBigMap t1 t2

singFromTOpq :: Sing t -> Sing (FromTOpq t)
singFromTOpq (STc t1) =
  withDict (singICT t1) $
  withDict (singTypeableCT t1) $
  Michelson.STc t1
singFromTOpq (STKey) = Michelson.STKey
singFromTOpq (STUnit) = Michelson.STUnit
singFromTOpq (STSignature) = Michelson.STSignature
singFromTOpq (STChainId) = Michelson.STChainId
singFromTOpq (STOption t1) =
  withDict (singIT t1) $
  withDict (singTypeableT t1) $
  Michelson.STOption t1
singFromTOpq (STList t1) =
  withDict (singIT t1) $
  withDict (singTypeableT t1) $
  Michelson.STList t1
singFromTOpq (STSet t1) =
  withDict (singICT t1) $
  withDict (singTypeableCT t1) $
  Michelson.STSet t1
singFromTOpq (STOperation) = Michelson.STOperation
singFromTOpq (STContract t1) =
  withDict (singIT t1) $
  withDict (singTypeableT t1) $
  Michelson.STContract t1
singFromTOpq (STLambda t1 t2) =
  withDict (singIT t1) $
  withDict (singIT t2) $
  withDict (singTypeableT t1) $
  withDict (singTypeableT t2) $
  Michelson.STLambda t1 t2
singFromTOpq (STMap t1 t2) =
  withDict (singICT t1) $
  withDict (singIT  t2) $
  withDict (singTypeableCT t1) $
  withDict (singTypeableT  t2) $
  Michelson.STMap t1 t2
singFromTOpq (STBigMap t1 t2) =
  withDict (singICT t1) $
  withDict (singIT  t2) $
  withDict (singTypeableCT t1) $
  withDict (singTypeableT  t2) $
  Michelson.STBigMap t1 t2

type family FromTAlg (t :: TAlg) :: T where
  FromTAlg ('TOpq t) = FromTOpq t
  FromTAlg ('TPair t1 t2) = 'Michelson.TPair (FromTAlg t1) (FromTAlg t2)
  FromTAlg ('TOr t1 t2) = 'Michelson.TOr (FromTAlg t1) (FromTAlg t2)

singFromTAlg :: Sing t -> Sing (FromTAlg t)
singFromTAlg (STOpq t) = singFromTOpq t
singFromTAlg (STPair ta tb) =
  withDict1 (singFromTAlg ta) $
  withDict1 (singFromTAlg tb) $
  withDict (singTypeableT (singFromTAlg ta)) $
  withDict (singTypeableT (singFromTAlg tb)) $
  Michelson.STPair (singFromTAlg ta) (singFromTAlg tb)
singFromTAlg (STOr ta tb) =
  withDict1 (singFromTAlg ta) $
  withDict1 (singFromTAlg tb) $
  withDict (singTypeableT (singFromTAlg ta)) $
  withDict (singTypeableT (singFromTAlg tb)) $
  Michelson.STOr (singFromTAlg ta) (singFromTAlg tb)

-- | Proof that `FromTAlg` `ToTAlg` is an isomorphism
fromToTAlg :: Sing t -> FromTAlg (ToTAlg t) :~: t
fromToTAlg (Michelson.STc _) = Refl
fromToTAlg (Michelson.STKey) = Refl
fromToTAlg (Michelson.STUnit) = Refl
fromToTAlg (Michelson.STSignature) = Refl
fromToTAlg (Michelson.STChainId) = Refl
fromToTAlg (Michelson.STOption _) = Refl
fromToTAlg (Michelson.STList _) = Refl
fromToTAlg (Michelson.STSet _) = Refl
fromToTAlg (Michelson.STOperation) = Refl
fromToTAlg (Michelson.STContract _) = Refl
fromToTAlg (Michelson.STLambda _ _) = Refl
fromToTAlg (Michelson.STMap _ _) = Refl
fromToTAlg (Michelson.STBigMap _ _) = Refl
fromToTAlg (Michelson.STPair t1 t2) =
  case (fromToTAlg t1, fromToTAlg t2) of
    (Refl, Refl) -> Refl
fromToTAlg (Michelson.STOr t1 t2) =
  case (fromToTAlg t1, fromToTAlg t2) of
    (Refl, Refl) -> Refl

-- | Proof that `FromTAlg` `ToTAlg` is an isomorphism
toFromTOpq :: Sing t -> ToTAlg (FromTOpq t) :~: 'TOpq t
toFromTOpq (STc _) = Refl
toFromTOpq (STKey) = Refl
toFromTOpq (STUnit) = Refl
toFromTOpq (STSignature) = Refl
toFromTOpq (STChainId) = Refl
toFromTOpq (STOption _) = Refl
toFromTOpq (STList _) = Refl
toFromTOpq (STSet _) = Refl
toFromTOpq (STOperation) = Refl
toFromTOpq (STContract _) = Refl
toFromTOpq (STLambda _ _) = Refl
toFromTOpq (STMap _ _) = Refl
toFromTOpq (STBigMap _ _) = Refl
-- toFromTAlg (STPair t1 t2) =
--   case (toFromTAlg t1, toFromTAlg t2) of
--     (Refl, Refl) -> Refl
-- toFromTAlg (Michelson.STOr t1 t2) =
--   case (toFromTAlg t1, toFromTAlg t2) of
--     (Refl, Refl) -> Refl




