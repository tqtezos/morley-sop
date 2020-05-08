{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.Value.Transformer where

import Control.Monad
import Data.Kind
import Prelude (($))

import Lorentz (Value)
import Michelson.Typed.T
import Michelson.Typed.Value
import Michelson.Typed.Instr (Instr)

type ValueT = ValueT' Instr

-- | Only considers recursions with `VPair`
data ValueT' instr (f :: Type -> Type) (t :: T) where
  -- VTNoPair :: NoPair t => f (Value' instr t) -> ValueT' instr f t
  VTPair :: f (ValueT' instr f ta) -> f (ValueT' instr f tb) -> ValueT' instr f ('TPair ta tb)

-- instance (Alternative f, SingI t) => Semigroup (ValueT f t) where
--   (<>) = _
--
-- instance (Alternative f, SingI t) => Monoid (ValueT f t) where
--   mempty =
--     case hasPair (sing @t) of
--       Left Dict -> VTNoPair empty
--       Right (HasPair sta stb) -> emptyValueT sta `VTPair` emptyValueT stb

-- emptyValueT :: Alternative f => Sing t -> ValueT f t
-- emptyValueT st =
--   case hasPair st of
--     Left Dict -> VTNoPair empty
--     Right (HasPair sta stb) -> VTPair empty empty -- _ (emptyValueT sta) `VTPair` emptyValueT stb

runValueT :: Monad m => ValueT m t -> m (Value t)
-- runValueT (VTNoPair xs) = xs
runValueT (VTPair xs ys) = do
  xs' <- xs >>= runValueT
  ys' <- ys >>= runValueT
  return $ VPair (xs', ys')

-- transValueT :: Functor g => (forall (t' :: T). f (Value t') -> g (Value t')) -> ValueT f t -> ValueT g t
-- transValueT trans' (VTNoPair xs) = VTNoPair $ trans' xs
-- transValueT trans' (VTPair xs ys) = (transValueT trans' <$> trans' xs) `VTPair` transValueT trans' ys

--   VCT :: Value ('Tc t) ->

--   VCT :: CValue t -> Value' instr ('Tc t)
--   VKeyT :: PublicKey -> Value' instr 'TKey
--   VUnit :: Value' instr 'TUnit
--   VSignature :: Signature -> Value' instr 'TSignature
--   VChainId :: ChainId -> Value' instr 'TChainId
--   VOption :: forall t instr. Maybe (Value' instr t) -> Value' instr ('TOption t)
--   VList :: forall t instr. [Value' instr t] -> Value' instr ('TList t)
--   VSet :: forall t instr. Set (CValue t) -> Value' instr ('TSet t)
--   VOp :: Operation' instr -> Value' instr 'TOperation
--   VContract :: forall arg instr. Address -> SomeEntryPointCallT arg -> Value' instr ('TContract arg)
--   VPair :: forall l r instr. (Value' instr l, Value' instr r) -> Value' instr ('TPair l r)
--   VOr :: forall l r instr. Either (Value' instr l) (Value' instr r) -> Value' instr ('TOr l r)
--   VLam
--     :: forall inp out instr.
--        ( forall i o.
--           ( Show (instr i o)
--           , Eq (instr i o)
--           )
--        )
--     => RemFail instr (inp ': '[]) (out ': '[]) -> Value' instr ('TLambda inp out)
--   VMap :: forall k v instr. Map (CValue k) (Value' instr v) -> Value' instr ('TMap k v)
--   VBigMap :: forall k v instr. Map (CValue k) (Value' instr v) -> Value' instr ('TBigMap k v)


--   VC :: CValue t -> Value' instr ('Tc t)
--   VKey :: PublicKey -> Value' instr 'TKey
--   VUnit :: Value' instr 'TUnit
--   VSignature :: Signature -> Value' instr 'TSignature
--   VChainId :: ChainId -> Value' instr 'TChainId
--   VOption :: forall t instr. Maybe (Value' instr t) -> Value' instr ('TOption t)
--   VList :: forall t instr. [Value' instr t] -> Value' instr ('TList t)
--   VSet :: forall t instr. Set (CValue t) -> Value' instr ('TSet t)
--   VOp :: Operation' instr -> Value' instr 'TOperation
--   VContract :: forall arg instr. Address -> SomeEntryPointCallT arg -> Value' instr ('TContract arg)
--   VPair :: forall l r instr. (Value' instr l, Value' instr r) -> Value' instr ('TPair l r)
--   VOr :: forall l r instr. Either (Value' instr l) (Value' instr r) -> Value' instr ('TOr l r)
--   VLam
--     :: forall inp out instr.
--        ( forall i o.
--           ( Show (instr i o)
--           , Eq (instr i o)
--           )
--        )
--     => RemFail instr (inp ': '[]) (out ': '[]) -> Value' instr ('TLambda inp out)
--   VMap :: forall k v instr. Map (CValue k) (Value' instr v) -> Value' instr ('TMap k v)
--   VBigMap :: forall k v instr. Map (CValue k) (Value' instr v) -> Value' instr ('TBigMap k v)

