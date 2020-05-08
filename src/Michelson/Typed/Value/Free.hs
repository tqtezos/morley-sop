{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.Value.Free where

import Data.Either
import Data.Semigroup
import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Function
import Data.Bifunctor
import Data.Eq
import Data.String
import Text.Show
import Data.Maybe

import Data.Aeson (ToJSON(..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Singletons
import Control.DeepSeq

import Michelson.Typed.Scope
import Michelson.Typed.Value (CValue, Value', Operation', RemFail)
import Michelson.Typed.Instr (Instr)
import Michelson.Typed.EntryPoints
import Morley.Micheline
import Tezos.Address
import Tezos.Crypto
import Tezos.Core
import qualified Michelson.Typed.Value as Michelson

import Data.Constraint.HasDict1
import Michelson.Typed.T.Alg



type ValueOpq = ValueOpq' Instr
type ValueAlg = ValueAlg' Instr
type ValueAlgT = ValueAlgT' Instr

data ValueOpq' instr t where
  VC :: CValue t -> ValueOpq' instr ('Tc t)
  VKey :: PublicKey -> ValueOpq' instr 'TKey
  VUnit :: ValueOpq' instr 'TUnit
  VSignature :: Signature -> ValueOpq' instr 'TSignature
  VChainId :: ChainId -> ValueOpq' instr 'TChainId
  VOption :: forall t instr. Maybe (Value' instr t) -> ValueOpq' instr ('TOption t)
  VList :: forall t instr. [Value' instr t] -> ValueOpq' instr ('TList t)
  VSet :: forall t instr. Set (CValue t) -> ValueOpq' instr ('TSet t)
  VOp :: Operation' instr -> ValueOpq' instr 'TOperation
  VContract :: forall arg instr. Address -> SomeEntryPointCallT arg -> ValueOpq' instr ('TContract arg)
  VLam
    :: forall inp out instr.
       ( forall i o.
          ( Show (instr i o)
          , Eq (instr i o)
          , NFData (instr i o)
          )
       )
    => RemFail instr (inp ': '[]) (out ': '[]) -> ValueOpq' instr ('TLambda inp out)
  VMap :: forall k v instr. Map (CValue k) (Value' instr v) -> ValueOpq' instr ('TMap k v)
  VBigMap :: forall k v instr. Map (CValue k) (Value' instr v) -> ValueOpq' instr ('TBigMap k v)

fromValueOpq :: ValueOpq' instr t -> Value' instr (FromTOpq t)
fromValueOpq (VC xs) = (Michelson.VC xs)
fromValueOpq (VKey xs) = (Michelson.VKey xs)
fromValueOpq (VUnit) = (Michelson.VUnit)
fromValueOpq (VSignature xs) = (Michelson.VSignature xs)
fromValueOpq (VChainId xs) = (Michelson.VChainId xs)
fromValueOpq (VOption xs) = (Michelson.VOption xs)
fromValueOpq (VList xs) = (Michelson.VList xs)
fromValueOpq (VSet xs) = (Michelson.VSet xs)
fromValueOpq (VOp xs) = (Michelson.VOp xs)
fromValueOpq (VContract xs ys) = (Michelson.VContract xs ys)
fromValueOpq (VLam xs) = (Michelson.VLam xs)
fromValueOpq (VMap xs) = (Michelson.VMap xs)
fromValueOpq (VBigMap xs) = (Michelson.VBigMap xs)

instance Show (ValueOpq' instr t) where
  show = show . fromValueOpq

instance (SingI t, HasNoOp (FromTOpq t)) => ToJSON (ValueOpq t) where
  toJSON = withDict1 (singFromTOpq (sing @t)) $ toJSON . toExpression . fromValueOpq

data ValueAlg' instr t where
  VPair :: forall l r instr. (ValueAlg' instr l, ValueAlg' instr r) -> ValueAlg' instr ('TPair l r)
  VOr :: forall l r instr. Either (ValueAlg' instr l) (ValueAlg' instr r) -> ValueAlg' instr ('TOr l r)

  VOpq :: forall t instr. ValueOpq' instr t -> ValueAlg' instr ('TOpq t)

toValueAlg :: Value' instr t -> ValueAlg' instr (ToTAlg t)
toValueAlg (Michelson.VC xs) = VOpq $ VC xs
toValueAlg (Michelson.VKey xs) = VOpq $ VKey xs
toValueAlg (Michelson.VUnit) = VOpq $ VUnit
toValueAlg (Michelson.VSignature xs) = VOpq $ VSignature xs
toValueAlg (Michelson.VChainId xs) = VOpq $ VChainId xs
toValueAlg (Michelson.VOption xs) = VOpq $ VOption xs
toValueAlg (Michelson.VList xs) = VOpq $ VList xs
toValueAlg (Michelson.VSet xs) = VOpq $ VSet xs
toValueAlg (Michelson.VOp xs) = VOpq $ VOp xs
toValueAlg (Michelson.VContract xs ys) = VOpq $ VContract xs ys
toValueAlg (Michelson.VPair xs) = VPair $ bimap toValueAlg toValueAlg xs
toValueAlg (Michelson.VOr xs) = VOr $ bimap toValueAlg toValueAlg xs
toValueAlg (Michelson.VLam xs) = VOpq $ VLam xs
toValueAlg (Michelson.VMap xs) = VOpq $ VMap xs
toValueAlg (Michelson.VBigMap xs) = VOpq $ VBigMap xs

-- | unused
fromValueAlg :: ValueAlg' instr t -> Value' instr (FromTAlg t)
fromValueAlg (VOpq (VC xs)) = Michelson.VC xs
fromValueAlg (VOpq (VKey xs)) = Michelson.VKey xs
fromValueAlg (VOpq (VUnit)) = Michelson.VUnit
fromValueAlg (VOpq (VSignature xs)) = Michelson.VSignature xs
fromValueAlg (VOpq (VChainId xs)) = Michelson.VChainId xs
fromValueAlg (VOpq (VOption xs)) = Michelson.VOption xs
fromValueAlg (VOpq (VList xs)) = Michelson.VList xs
fromValueAlg (VOpq (VSet xs)) = Michelson.VSet xs
fromValueAlg (VOpq (VOp xs)) = Michelson.VOp xs
fromValueAlg (VOpq (VContract xs ys)) = Michelson.VContract xs ys
fromValueAlg (VPair xs) = Michelson.VPair $ bimap fromValueAlg fromValueAlg xs
fromValueAlg (VOr xs) = Michelson.VOr $ bimap fromValueAlg fromValueAlg xs
fromValueAlg (VOpq (VLam xs)) = Michelson.VLam xs
fromValueAlg (VOpq (VMap xs)) = Michelson.VMap xs
fromValueAlg (VOpq (VBigMap xs)) = Michelson.VBigMap xs

instance SingI t => Show (ValueAlg' instr t) where
  show = show . fromValueAlg

data ValueAlgT' instr f t where
  VTPair :: forall l r instr f. (ValueAlgT' instr f l, ValueAlgT' instr f r) -> ValueAlgT' instr f ('TPair l r)
  VTOr :: forall l r instr f. (ValueAlgT' instr f l, ValueAlgT' instr f r) -> ValueAlgT' instr f ('TOr l r)

  VTOpq :: forall t instr f. f (ValueOpq' instr t) -> ValueAlgT' instr f ('TOpq t)

toValueAlgT ::
     forall instr f t. MonadFail f
  => Sing t
  -> ValueAlg' instr t
  -> ValueAlgT' instr f t
toValueAlgT (STPair sta stb) (VPair (xs, ys)) =
  VTPair (toValueAlgT sta xs, toValueAlgT stb ys)
toValueAlgT (STOr sta stb) (VOr xss) =
  VTOr $
  case xss of
    Left xs -> (toValueAlgT sta xs, failValueAlgT "toValueAlgT" stb)
    Right ys -> (failValueAlgT "toValueAlgT" sta, toValueAlgT stb ys)
toValueAlgT _ (VOpq xs) = VTOpq $ pure xs

-- | Calculate a `ValueAlgT` from its type
pureValueAlgT :: forall instr f t. ()
  => (forall a. Proxy a -> f a)
  -> Sing t
  -> ValueAlgT' instr f t
pureValueAlgT f (STPair ta tb) = VTPair (pureValueAlgT f ta, pureValueAlgT f tb)
pureValueAlgT f (STOr ta tb) = VTOr (pureValueAlgT f ta, pureValueAlgT f tb)
pureValueAlgT f (STOpq _st) = VTOpq $ f Proxy

-- | `pureValueAlgT` using `fail` to provide instances of @f@
failValueAlgT :: forall instr f t. MonadFail f
  => String
  -> Sing t
  -> ValueAlgT' instr f t
failValueAlgT err = pureValueAlgT $ const $ fail $ "failValueAlgT: " <> err

runValueAlgT :: forall instr f t. Alternative f
  => ValueAlgT' instr f t
  -> f (ValueAlg' instr t)
runValueAlgT (VTPair (xs, ys)) = fmap VPair $ (,) <$> runValueAlgT xs <*> runValueAlgT ys
runValueAlgT (VTOr (xs, ys)) = fmap VOr $ (Left <$> runValueAlgT xs) <|> (Right <$> runValueAlgT ys)
runValueAlgT (VTOpq xs) = VOpq <$> xs

