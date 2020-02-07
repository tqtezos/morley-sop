{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeInType #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Lorentz.Contracts.Value where

import Prelude (Constraint) -- Eq(..), Show(..), Constraint, Map, ($))
-- import Data.Foldable
-- import Data.Maybe
-- import Data.Either
-- import Data.Functor
-- import Data.Kind
-- import Data.Traversable
import GHC.TypeLits
import Data.Ord

-- import Michelson.Typed.Haskell
-- import Michelson.Typed.Value
-- import Michelson.Typed.Instr hiding (LT)
-- import Michelson.Typed.Sing
-- import Michelson.Typed.T
-- import Michelson.Typed.Aliases

-- import Data.Constraint
-- import Data.Singletons
-- import Data.Singletons.Prelude.List (Sing(..)) -- SCons, SNil)
-- import Data.SOP (SOP(..), NS(..), NP(..), I(..), All, SListI, SListI2)
-- import qualified Data.SOP as SOP
-- import Generics.SOP (Generic(..))

-- import Michelson.Typed.Value.SOP

-- data TF a =
--     TFc CT
--   | TFKey
--   -- | TFUnit -- empty sum
--   | TFSignature
--   -- | TFOption (TF a) -- converted to SOP
--   | TFList (TF a)
--   | TFSet CT
--   | TFOperation
--   | TFContract T
--   -- | TFPair T T -- converted to SOP
--   -- | TFOr T T -- converted to SOP
--   | TFLambda T T
--   | TFMap CT (TF a)
--   | TFBigMap CT (TF a)
--   deriving (Eq, Show, Functor, Foldable, Traversable)
--
-- data ValueF' (f :: a -> Type) instr (tf :: TF a) where
--   ValueTFc' :: Value' instr ('Tc ct) -> ValueF' f instr ('TFc ct)
--   ValueTFKey' :: Value' instr 'TKey -> ValueF' f instr 'TFKey
--   ValueTFSignature' :: Value' instr 'TSignature -> ValueF' f instr 'TFSignature
--   ValueTFList' :: [ValueF' f instr xs] -> ValueF' f instr ('TFList xs)
--   ValueTFSet' :: Value' instr ('TSet ct) -> ValueF' f instr ('TFSet ct)
--   ValueTFOperation' :: Value' instr 'TOperation -> ValueF' f instr 'TFOperation
--   ValueTFContract' :: Value' instr ('TContract t) -> ValueF' f instr ('TFContract t)
--   ValueTFLambda' :: Value' instr ('TLambda a b) -> ValueF' f instr ('TFLambda a b)
--   ValueTFMap' :: Map (CValue ct) (ValueF' f instr t) -> ValueF' f instr ('TFMap ct t)
--   ValueTFBigMap' :: BigMap (CValue ct) (ValueF' f instr t) -> ValueF' f instr ('TFBigMap ct t)

type SymbolMap a = [(Symbol, a)]

type family StripFst (xs :: [(t, a)]) :: [a] where
  StripFst '[] = '[]
  StripFst (('(,) _ x) ': xs) = x ': StripFst xs

type family StripFst2 (xs :: [(t, [(t, a)])]) :: [[a]] where
  StripFst2 '[] = '[]
  StripFst2 (('(,) _ x) ': xs) = StripFst x ': StripFst2 xs

type family ValidSymbolMap (xs :: SymbolMap a) :: Constraint where
  ValidSymbolMap '[] = ()
  ValidSymbolMap (_ ': '[]) = ()
  ValidSymbolMap (('(,) x _) ': (('(,) y ys) ': zs)) = (CmpSymbol x y ~ 'LT, ValidSymbolMap (('(,) y ys) ': zs))

type family ValidSymbolMap2 (xs :: SymbolMap (SymbolMap a)) :: Constraint where
  ValidSymbolMap2 '[] = ()
  ValidSymbolMap2 (('(,) _ xs) ': '[]) = ValidSymbolMap xs
  ValidSymbolMap2 (('(,) x xs) ': (('(,) y ys) ': zs)) = (CmpSymbol x y ~ 'LT, ValidSymbolMap xs, ValidSymbolMap ys, ValidSymbolMap2 (('(,) y ys) ': zs))


-- data TAnn where
--   -- TAnn :: [(Symbol, [(Symbol, TF TAnn)])] -> TAnn
--   TAnn :: SymbolMap (SymbolMap (TF TAnn)) -> TAnn
--   deriving (Eq, Show)
--
-- data SomeTAnn' instr where
--   SomeTAnn' :: forall instr t (ta :: TAnn). ValidTAnn' instr t ta => Sing t -> Sing ta -> SomeTAnn' instr
--
-- type SomeTAnn = SomeTAnn' Instr

-- someTAnn :: Parsed.T -> SomeTAnn
-- someTAnn = _

-- data Value' instr t where
--   VC :: CValue t -> Value' instr ('Tc t)
--   VKey :: PublicKey -> Value' instr 'TKey
--   VUnit :: Value' instr 'TUnit
--   VSignature :: Signature -> Value' instr 'TSignature
--   VChainId :: ChainId -> Value' instr 'TChainId
--   VOption :: forall t instr. Maybe (Value' instr t) -> Value' instr ('TOption t)
--   VList :: forall t instr. [Value' instr t] -> Value' instr ('TList t)
--   VSet :: forall t instr. Set (CValue t) -> Value' instr ('TSet t)
--   VOp :: Operation' instr -> Value' instr 'TOperation
--   VContract :: forall p instr. Address -> Value' instr ('TContract p)
--   VPair :: forall l r instr. (Value' instr l, Value' instr r) -> Value' instr ('TPair l r)
--   VOr :: forall l r instr. Either (Value' instr l) (Value' instr r) -> Value' instr ('TOr l r)
--   VLam :: forall inp out instr.  ( Show (instr '[inp] '[out]) , Eq (instr '[inp] '[out])) => instr (inp ': '[]) (out ': '[]) -> Value' instr ('TLambda inp out)
--   VMap :: forall k v instr. Map (CValue k) (Value' instr v) -> Value' instr ('TMap k v)
--   VBigMap :: forall k v instr. Map (CValue k) (Value' instr v) -> Value' instr ('TBigMap k v)

-- type family AllTGeneric (xs :: [[T]]) :: Constraint where
--   AllTGeneric '[] = ()
--   AllTGeneric ('[] ': xs) = AllTGeneric xs
--   AllTGeneric ((x ': xs) ': ys) = (SingI x, AllTGeneric (xs ': ys))
  -- [TGeneric instr y | x <- xs, y <- x]

-- Generic (Value' instr t),
-- Code (Value' instr t) ~ Value2' instr (TCode t),
-- class (SingI t, AllTGeneric (TCode t)) => TGeneric instr (t :: T) where

-- isTGeneric :: SingI t :- TGeneric instr t

-- type family ValidTAnn' instr (t :: T) (ta :: TAnn) :: Constraint where
--   -- ValidTAnn' instr t ('TAnn ta) = (TGeneric instr t, ValidSymbolMap ta, ValidTAnnTCode (TCode t) ta)
--   ValidTAnn' instr t ('TAnn ta) = (SingI t, ValidSymbolMap ta, ValidTAnnTCode (TCode t) ta)
--
-- type family ValidTAnnTCode (tc :: [[T]]) (ta :: SymbolMap (SymbolMap (TF TAnn))) :: Constraint where
--   -- zipWith (zipWith ==) ..
--
-- data ValueAnn' (instr :: [T] -> [T] -> Type) (ta :: TAnn) where
--   ValueAnn :: forall (instr :: [T] -> [T] -> Type) (ta :: SymbolMap (SymbolMap (TF TAnn))).
--        Sing ta
--     -> SOP (ValueF' (ValueAnn' instr) instr) (StripFst2 ta)
--     -> ValueAnn' instr ('TAnn ta)
--
-- runValueAnn' :: forall instr t ta. ValidTAnn' instr t ta => ValueAnn' instr ta -> Value' instr t
-- runValueAnn' (ValueAnn sta (SOP xs)) = _
  -- case xs of
  --   SOP.Z ys ->
  --     case ys of
  --       SOP.Nil ->
  --         _ (sing @t) sta
  --       (SOP.:*) z zs ->
  --         _ (sing @t) sta z zs
  --   SOP.S xs' ->
  --     _ (sing @t) sta xs'

-- parseValueAnn :: forall t ta. ValidTAnn t ta => Opt.Parser (ValueAnn ta)

-- json/aeson parsing of values


-- data T =
--     Tc CT
--   | TKey
--   | TUnit
--   | TSignature
--   | TOption T
--   | TList T
--   | TSet CT
--   | TOperation
--   | TContract T
--   | TPair T T
--   | TOr T T
--   | TLambda T T
--   | TMap CT T
--   | TBigMap CT T
--   deriving (Eq, Show)


-- instance SingKind CT where
--   type Demote CT = CT
--   fromSing  = fromSingCT
--   toSing t = case toSingCT t of SomeSingCT s -> SomeSing s

-- -- | Instance of data family 'Sing' for 'T'.
-- -- Custom instance is implemented in order to inject 'Typeable'
-- -- constraint for some of constructors.
-- data instance Sing :: T -> Type where
--   STc :: (SingI a, Typeable a) => Sing a -> Sing ( 'Tc a)
--   STKey :: Sing  'TKey
--   STUnit :: Sing  'TUnit
--   STSignature :: Sing  'TSignature
--   STChainId :: Sing  'TChainId
--   STOption :: (SingI a, Typeable a) => Sing a -> Sing ( 'TOption a)
--   STList :: (SingI a, Typeable a) => Sing a -> Sing ( 'TList a )
--   STSet :: (SingI a, Typeable a) => Sing a -> Sing ( 'TSet a )
--   STOperation  :: Sing 'TOperation
--   STContract   :: (SingI a, Typeable a)
--                 => Sing a -> Sing ( 'TContract a )
--   STPair       :: (SingI a, SingI b, Typeable a, Typeable b)
--                 => Sing a -> Sing b -> Sing ('TPair a b)
--   STOr         :: (SingI a, SingI b, Typeable a, Typeable b)
--                 => Sing a -> Sing b -> Sing ('TOr a b)
--   STLambda     :: (SingI a, SingI b, Typeable a, Typeable b)
--                 => Sing a -> Sing b -> Sing ('TLambda a b)
--   STMap        :: (SingI a, SingI b, Typeable a, Typeable b)
--                 => Sing a -> Sing b -> Sing ('TMap a b)
--   STBigMap    :: (SingI a, SingI b, Typeable a, Typeable b)
--                 => Sing a -> Sing b -> Sing ('TBigMap a b)


-- data TF =
--     TFc CT
--   | TFKey
--   | TFUnit
--   | TFSignature
--   | TFChainId
--   | TFOption Type
--   | TFList Type
--   | TFSet Comparable
--   | TFOperation
--   | TFContract Type
--   | TFPair FieldAnn FieldAnn Type Type
--   | TFOr FieldAnn FieldAnn Type Type
--   | TFLambda Type Type
--   | TFMap Comparable Type
--   | TFBigMap Comparable Type
--   deriving (Eq, Show, Data, Generic)

-- data T =
--     Tc CT
--   | TKey
--   | TUnit
--   | TSignature
--   | TChainId
--   | TOption Type
--   | TList Type
--   | TSet Comparable
--   | TOperation
--   | TContract Type
--   | TPair FieldAnn FieldAnn Type Type
--   | TOr FieldAnn FieldAnn Type Type
--   | TLambda Type Type
--   | TMap Comparable Type
--   | TBigMap Comparable Type
--   deriving (Eq, Show, Data, Generic)



