
module Michelson.Typed.Value.Arbitrary where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Functor
import Data.Type.Equality

import Michelson.Typed.T
import qualified Michelson.Typed.T as Michelson
import Michelson.Typed.Sing
import Michelson.Typed.Value
import Michelson.Typed.Instr
import Michelson.Typed.Scope
import Michelson.Typed.EntryPoints
import Michelson.Typed.Annotation
import Michelson.Untyped.Annotation

import Util.Test.Arbitrary
import Michelson.Test.Gen

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Data.Constraint
import Data.Singletons

import Data.Constraint.HasDict1
import Michelson.Typed.T.Alg (TAlg)
import qualified Michelson.Typed.T.Alg as Alg
import Michelson.Typed.T.Sing
import Michelson.Typed.Annotation.Sing
import Michelson.Typed.Annotation.Sing.Alg
import Michelson.Typed.Annotation.Sing.Notes

-- tt = _


allTs :: forall f. Applicative f => f CT -> f T -> [f T]
allTs xs fs =
  [ Michelson.Tc <$> xs
  , pure Michelson.TKey
  , pure Michelson.TUnit
  , pure Michelson.TSignature
  , pure Michelson.TChainId
  , Michelson.TOption <$> fs
  , Michelson.TList <$> fs
  , Michelson.TSet <$> xs
  , pure Michelson.TOperation
  , Michelson.TContract <$> fs
  , Michelson.TPair <$> fs <*> fs
  , Michelson.TOr <$> fs <*> fs
  , Michelson.TLambda <$> fs <*> fs
  , Michelson.TMap <$> xs <*> fs
  , Michelson.TBigMap <$> xs <*> fs
  ]

allTAlgs :: forall f. Applicative f => f CT -> f T -> f TAlg -> [f TAlg]
allTAlgs xs fs gs =
  [ Alg.TOpq . Alg.Tc <$> xs
  , pure (Alg.TOpq Alg.TKey)
  , pure (Alg.TOpq Alg.TUnit)
  , pure (Alg.TOpq Alg.TSignature)
  , pure (Alg.TOpq Alg.TChainId)
  , Alg.TOpq . Alg.TOption <$> fs
  , Alg.TOpq . Alg.TList <$> fs
  , Alg.TOpq . Alg.TSet <$> xs
  , pure (Alg.TOpq Alg.TOperation)
  , Alg.TOpq . Alg.TContract <$> fs
  , Alg.TPair <$> gs <*> gs
  , Alg.TOr <$> gs <*> gs
  , fmap Alg.TOpq . Alg.TLambda <$> fs <*> fs
  , fmap Alg.TOpq . Alg.TMap <$> xs <*> fs
  , fmap Alg.TOpq . Alg.TBigMap <$> xs <*> fs
  ]

instance Arbitrary T where
  arbitrary = oneof $ allTs arbitrary arbitrary

  -- [ Michelson.Tc <$> xs
  -- , pure Michelson.TKey
  -- , pure Michelson.TUnit
  shrink Michelson.TSignature = [Michelson.TUnit]
  shrink Michelson.TChainId = [Michelson.TUnit]
  shrink (Michelson.Tc _) = [Michelson.TUnit]
  shrink (Michelson.TOption xs) = xs : fmap Michelson.TOption (shrink xs)
  shrink (Michelson.TList xs) = xs : fmap Michelson.TList (shrink xs)
  shrink (Michelson.TSet xs) = [Michelson.Tc xs]
  -- , pure Michelson.TOperation
  shrink (Michelson.TContract xs) = xs : fmap Michelson.TContract (shrink xs)
  shrink (Michelson.TPair xs ys) = xs : ys : (Michelson.TPair <$> shrink xs <*> shrink ys)
  shrink (Michelson.TOr xs ys) = xs : ys : (Michelson.TOr <$> shrink xs <*> shrink ys)
  shrink (Michelson.TLambda xs ys) = xs : ys : (Michelson.TLambda <$> shrink xs <*> shrink ys)
  shrink (Michelson.TMap xs ys) = Michelson.Tc xs : ys : (Michelson.TMap <$> pure xs <*> shrink ys)
  shrink (Michelson.TBigMap xs ys) = Michelson.Tc xs : ys : (Michelson.TBigMap <$> pure xs <*> shrink ys)
  shrink _ = []

instance Arbitrary TAlg where
  arbitrary = oneof $ allTAlgs arbitrary arbitrary arbitrary

instance Arbitrary (CValue 'CNat) where
  arbitrary = CvNat <$> arbitrary
  shrink (CvNat xs) = CvNat <$> shrink xs

instance Arbitrary (CValue 'CString) where
  arbitrary = CvString <$> arbitrary
  shrink (CvString xs) = CvString <$> shrink xs

instance Arbitrary (CValue 'CBytes) where
  arbitrary = CvBytes <$> arbitrary
  shrink (CvBytes xs) = CvBytes <$> shrink xs

instance Arbitrary (CValue 'CBool) where
  arbitrary = CvBool <$> arbitrary
  shrink (CvBool xs) = CvBool <$> shrink xs

instance Arbitrary (CValue 'CAddress) where
  arbitrary = CvAddress <$> arbitrary
  shrink (CvAddress xs) = CvAddress <$> shrink xs

prfArbitraryCValue :: Sing t -> Dict (Arbitrary (CValue t))
prfArbitraryCValue =
  \case
    SCInt -> Dict
    SCNat -> Dict
    SCString -> Dict
    SCBytes -> Dict
    SCMutez -> Dict
    SCBool -> Dict
    SCKeyHash -> Dict
    SCTimestamp -> Dict
    SCAddress -> Dict

-- | Note:
-- - `VLam` is `FAILWITH`
-- - Assertions are used since `ParameterScope` is opaque: `assertNestedBigMapsAbsense` and `assertOpAbsense`
instance (instr ~ Instr, SingI ts, ParameterScope ts) => Arbitrary (Value' instr ts) where
  arbitrary =
    case sing @ts of
      STc t -> withDict (prfArbitraryCValue t) $ VC <$> arbitrary
      STKey -> VKey <$> arbitrary
      STUnit -> return VUnit
      STSignature -> VSignature <$> arbitrary
      STChainId -> VChainId <$> arbitrary
      STOption t -> VOption <$> arbitrary
      STList t -> VList <$> arbitrary
      STSet t -> withDict (prfArbitraryCValue t) $ VSet <$> arbitrary
      -- STOperation -> _
      STContract arg -> VContract <$> arbitrary <*> return sepcCallRootUnsafe
      STPair l r -> assertNestedBigMapsAbsense l $ assertNestedBigMapsAbsense r $ assertOpAbsense l $ assertOpAbsense r $ VPair <$> arbitrary
      STOr l r -> assertNestedBigMapsAbsense l $ assertNestedBigMapsAbsense r $ assertOpAbsense l $ assertOpAbsense r $ VOr <$> arbitrary
      STLambda inp out -> return $ VLam (RfAlwaysFails FAILWITH)
      STMap k v -> withDict (prfArbitraryCValue k) $ VMap <$> arbitrary
      STBigMap k v -> withDict (prfArbitraryCValue k) $ assertNestedBigMapsAbsense v $ VBigMap <$> arbitrary

instance (Arbitrary a, SingI t) => Arbitrary (Annotated a t) where
  arbitrary = traverseAnnotated (const arbitrary) (annotatedFromNotes (starNotes @t))

  shrink = traverseAnnotated shrink

instance (Arbitrary a, SingI t) => Arbitrary (AnnotatedAlg a t) where
  arbitrary =
    case Alg.toFromTAlg (sing @t) of
      Refl ->
        withDict1 (Alg.singFromTAlg (sing @t)) $
        traverseAnnotatedAlg (const arbitrary) $
        case toSing (annotatedFromNotes (starNotes @(Alg.FromTAlg t))) of
          SomeSing sann -> fromSing $ singToAnnotatedAlg sann

  shrink = traverseAnnotatedAlg shrink


-- AltExcept
--   [ "(<||>) (PureAltE _) (PureAltE _):"
--   , Z (EpFields
--        ((:*) ((:+) "_1" Here) Here)
--        (EpField "_1" (RunPureAltE (Comp1 {unComp1 = I VUnit})) :*
--         EpField
--           "_2"
--           (RunPureAltE
--              (Comp1
--                 { unComp1 =
--                     I VC (CvTimestamp (Timestamp {unTimestamp = 1687812869 s}))
--                 })) :*
--         Nil))
--   , S (Z (EpFields
--           ((:*) ((:+) "_2" Here) Here)
--           (EpField
--              "_2"
--              (RunAltExcept
--                 (WrapSing
--                    { unwrapSing =
--                        SCons
--                          (SSym @"(<||>) (PureAltE _) (PureAltE _):")
--                          (SCons
--                             (SSym @"TUnit")
--                             (SCons (SSym @"Tc CTimestamp") SNil))
--                    })) :*
--            EpField
--              "_2"
--              (RunAltExcept
--                 (WrapSing
--                    { unwrapSing =
--                        SCons
--                          (SSym @"(<||>) (PureAltE _) (PureAltE _):")
--                          (SCons
--                             (SSym @"TUnit")
--                             (SCons (SSym @"Tc CTimestamp") SNil))
--                    })) :*
--            Nil)))
--   ]


