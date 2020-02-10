{-# OPTIONS -Wno-missing-export-lists -Wall -Wno-orphans #-}

module Michelson.Typed.Value.SOP where

import Prelude (($))
-- import Data.Foldable
-- import Data.Maybe
import Data.Either
-- import Data.Functor
-- import Data.Kind
-- import Data.Traversable
-- import GHC.TypeLits
-- import Data.Ord

-- import Michelson.Typed.Haskell
import Michelson.Typed.Value
import Michelson.Typed.Instr (Instr)
import Michelson.Typed.Sing
import Michelson.Typed.T
-- import Michelson.Typed.Aliases

import Data.Constraint
import Data.Singletons
import Data.Singletons.Prelude.List (Sing(..)) -- SCons, SNil)
import Data.SOP (SOP(..)) -- , NS(..), NP(..), I(..), All, SListI, SListI2)
import qualified Data.SOP as SOP
-- import Generics.SOP (Generic(..))

import Data.Constraint.HasDict1
import Data.SOP.Deep
import Data.SOP.Deep.Annotated
import Data.SOP.Deep.Annotated.Test


-- | `HasDict1` for `CT`
singICT :: forall (t :: CT). Sing t -> Dict (SingI t)
singICT SCInt = Dict
singICT SCNat = Dict
singICT SCString = Dict
singICT SCBytes = Dict
singICT SCMutez = Dict
singICT SCBool = Dict
singICT SCKeyHash = Dict
singICT SCTimestamp = Dict
singICT SCAddress = Dict

-- | `HasDict1` for `T`
singIT :: forall (t :: T). Sing t -> Dict (SingI t)
singIT (STc ct) =
  withDict (singICT ct) $
  Dict
singIT STKey = Dict
singIT STUnit = Dict
singIT STSignature = Dict
singIT STChainId = Dict
singIT (STOption st) =
  withDict (singIT st) $
  Dict
singIT (STList st) =
  withDict (singIT st) $
  Dict
singIT (STSet st) =
  withDict (singICT st) $
  Dict
singIT STOperation  = Dict
singIT (STContract st) =
  withDict (singIT st) $
  Dict
singIT (STPair st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STOr st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STLambda st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STMap st su) =
  withDict (singICT st) $
  withDict (singIT su) $
  Dict
singIT (STBigMap st su) =
  withDict (singICT st) $
  withDict (singIT su) $
  Dict

instance HasDict1 T where
  evidence1 = singIT

instance HasDeep T (Value' Instr) where
  type DCode (t :: T) = TCode t
  singDCode = singTCode
  toD = toValue'
  fromD = fromValue'

-- | `DCode` for `T`
type family TCode (t :: T) :: [[T]] where
  TCode ('Tc ct) = '[ '[ 'Tc ct]]
  TCode ('TKey) = '[ '[ 'TKey]]
  TCode ('TUnit) = '[ '[]] -- empty product
  TCode ('TSignature) = '[ '[ 'TSignature]]
  TCode ('TChainId) = '[ '[ 'TChainId]]
  TCode ('TOption t) = '[ '[ 'TOption t]] -- TCode ('TOption t) = '[] ': TCode t
  TCode ('TList t) = '[ '[ 'TList t]]
  TCode ('TSet ct) = '[ '[ 'TSet ct]]
  TCode ('TOperation) = '[ '[ 'TOperation]]
  TCode ('TContract t) = '[ '[ 'TContract t]]
  TCode ('TPair a b) = '[ '[a, b]] -- TCode a ** TCode b -- cartesian product
  TCode ('TOr a b) = '[ '[a], '[b]] -- TCode a ++ TCode b
  TCode ('TLambda a b) = '[ '[ 'TLambda a b]]
  TCode ('TMap a b) = '[ '[ 'TMap a b]]
  TCode ('TBigMap a b) = '[ '[ 'TBigMap a b]]

-- | `TCode` preserves `Sing`
singTCode :: forall (t :: T). Sing t -> Sing (TCode t)
singTCode (STc ct) = SCons (SCons (STc ct) SNil) SNil
singTCode STKey = SCons (SCons STKey SNil) SNil
singTCode STUnit = SCons SNil SNil
singTCode STSignature = SCons (SCons STSignature SNil) SNil
singTCode STChainId = SCons (SCons STChainId SNil) SNil
singTCode (STOption st) = SCons (SCons (STOption st) SNil) SNil
singTCode (STList st) = SCons (SCons (STList st) SNil) SNil
singTCode (STSet st) = SCons (SCons (STSet st) SNil) SNil
singTCode STOperation = SCons (SCons STOperation SNil) SNil
singTCode (STContract st) = SCons (SCons (STContract st) SNil) SNil
singTCode (STPair st su) = SCons (SCons st (SCons su SNil)) SNil
singTCode (STOr st su) = SCons (SCons st SNil) (SCons (SCons su SNil) SNil)
singTCode (STLambda st su) = SCons (SCons (STLambda st su) SNil) SNil
singTCode (STMap st su) = SCons (SCons (STMap st su) SNil) SNil
singTCode (STBigMap st su) = SCons (SCons (STBigMap st su) SNil) SNil

-- | Conversion from a shallow `SOP` of `Value'` to `Value'`.
--
-- See `fromValue'`
toValue' :: forall instr t. Sing t -> SOP (Value' instr) (TCode t) -> Value' instr t
toValue' st xs =
  case st of
    STc _ct ->
      case xs of
        SOP (SOP.Z ((SOP.:*) ys SOP.Nil)) -> ys
        SOP (SOP.S ys) ->
          case ys of
    STKey ->
      case xs of
        SOP (SOP.Z ((SOP.:*) ys SOP.Nil)) -> ys
        SOP (SOP.S ys) ->
          case ys of
    STUnit ->
      case xs of
        SOP (SOP.Z SOP.Nil) -> VUnit
        SOP (SOP.S ys) ->
          case ys of
    STSignature ->
      case xs of
        SOP (SOP.Z ((SOP.:*) ys SOP.Nil)) -> ys
        SOP (SOP.S ys) ->
          case ys of
    STChainId ->
      case xs of
        SOP (SOP.Z ((SOP.:*) ys SOP.Nil)) -> ys
        SOP (SOP.S ys) ->
          case ys of
    STOption (_ :: Sing t') ->
      case xs of
        SOP (SOP.Z ((SOP.:*) ys SOP.Nil)) -> ys
        SOP (SOP.S ys) ->
          case ys of
      -- case xs of
      --   SOP (SOP.Z SOP.Nil) -> VOption Nothing
      --   SOP (SOP.S xs') -> VOption $ Just $
      --     toValue' @instr @t' (SOP xs')
    STList _t ->
      case xs of
        SOP (SOP.Z ((SOP.:*) ys SOP.Nil)) -> ys
        SOP (SOP.S ys) ->
          case ys of
    STSet _ct ->
      case xs of
        SOP (SOP.Z ((SOP.:*) ys SOP.Nil)) -> ys
        SOP (SOP.S ys) ->
          case ys of
    STOperation ->
      case xs of
        SOP (SOP.Z ((SOP.:*) ys SOP.Nil)) -> ys
        SOP (SOP.S ys) ->
          case ys of
    STContract _t ->
      case xs of
        SOP (SOP.Z ((SOP.:*) ys SOP.Nil)) -> ys
        SOP (SOP.S ys) ->
          case ys of
    STPair _a _b ->
      case xs of
        SOP (SOP.Z ((SOP.:*) ys ((SOP.:*) zs SOP.Nil))) ->
          VPair (ys, zs)
        SOP (SOP.S ys) ->
          case ys of
    STOr _a _b ->
      case xs of
        SOP (SOP.Z ((SOP.:*) ys SOP.Nil)) ->
          VOr $ Left ys
        SOP (SOP.S (SOP.Z ((SOP.:*) ys SOP.Nil))) ->
          VOr $ Right ys
        SOP (SOP.S (SOP.S ys)) ->
          case ys of
      -- _ a b
    STLambda _a _b ->
      case xs of
        SOP (SOP.Z ((SOP.:*) ys SOP.Nil)) -> ys
        SOP (SOP.S ys) ->
          case ys of
    STMap _a _b ->
      case xs of
        SOP (SOP.Z ((SOP.:*) ys SOP.Nil)) -> ys
        SOP (SOP.S ys) ->
          case ys of
    STBigMap _a _b ->
      case xs of
        SOP (SOP.Z ((SOP.:*) ys SOP.Nil)) -> ys
        SOP (SOP.S ys) ->
          case ys of

-- | Conversion from `Value'` to a shallow `SOP` representation:
-- only the uppermost layer of sums and products are converted to
-- a `SOP` form.
fromValue' :: forall instr t. Sing t -> Value' instr t -> SOP (Value' instr) (TCode t)
fromValue' st (VC xs) =
  case st of
    STc _ct -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ VC xs
fromValue' st (VKey xs) =
  case st of
    STKey -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ (VKey xs)
fromValue' st (VUnit) =
  case st of
    STUnit -> SOP $ SOP.Z $ SOP.Nil
fromValue' st (VSignature xs) =
  case st of
    STSignature -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ (VSignature xs)
fromValue' st (VChainId xs) =
  case st of
    STChainId -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ (VChainId xs)
fromValue' st (VOption xs) =
  case st of
    STOption _st ->
      SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ (VOption xs)
  -- case xs of
  --   Nothing ->
  --     SOP $ SOP.Z $ SOP.Nil
  --   Just xs' ->
  --     case st of
  --       STOption _st ->
  --         case fromValue' xs' of
  --           SOP ys ->
  --             SOP $ SOP.S ys
fromValue' st (VList xs) =
  case st of
    (STList _t) -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ (VList xs)
fromValue' st (VSet xs) =
  case st of
    (STSet _t) -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ (VSet xs)
fromValue' st (VOp xs) =
  case st of
    STOperation -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ (VOp xs)
fromValue' st (VContract addr xs) =
  case st of
    (STContract _p) -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ (VContract addr xs)
fromValue' st (VPair (xs, ys)) =
  case st of
    STPair _sl _sr ->
      SOP (SOP.Z ((SOP.:*) xs ((SOP.:*) ys SOP.Nil)))
fromValue' st (VOr xs) =
  case st of
    STOr _stl _str ->
      case xs of
        Left ys ->
          SOP (SOP.Z ((SOP.:*) ys SOP.Nil))
        Right ys ->
          SOP (SOP.S (SOP.Z ((SOP.:*) ys SOP.Nil)))
fromValue' st (VLam xs) =
  case st of
    (STLambda _inp _out) -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ (VLam xs)
fromValue' st (VMap xs) =
  case st of
    (STMap _k _v) -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ (VMap xs)
fromValue' st (VBigMap xs) =
  case st of
    (STBigMap _k _v) -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ (VBigMap xs)



-- | select NS element with (K Int)
--   order product elements with (IntMap)
--   type-check product elements with expected type
--   if they don't match, throw an exception
--   then recurse on further elements
--
-- resolveNames :: (SingI t, MonadFail m) => m name -> Free (K name :*: Map name) (SomeValueF' instr) -> m (Free (K Int :*: IntMap) (SomeValueF' instr))
--
-- parseValueRecInt' :: (SingI t, MonadFail m) => Free (K Int :*: IntMap) (SomeValueF' instr) -> m (ValueRec' instr t)
--
-- parseValueRec' :: (SingI t, MonadFail m) => m name -> Free (K name :*: Map name) (SomeValueF' instr) -> m (ValueRec' instr t)
--
-- parseValue :: (SingI t, MonadFail m) => m name -> Free (K name :*: Map name) (SomeValueF' instr) -> m (Value' instr t)
--
-- unparseValueRec' :: (SingI t, Applicative m) => m name -> ValueRec' instr t -> m (Free (K name :*: Map name) (SomeValueF' instr))
--
-- unparseValue' :: (SingI t, Applicative m) => m name -> Value' instr t -> m (Free (K name :*: Map name) (SomeValueF' instr))
--
-- Foo instr t = SOP (SomeTFValue' ((Value' :+: Foo) instr)) (DeepTCode t)
--
-- SOP -> SomeTFValue', which should accept a handler for the recursive case
--   In the non-recursive case, we're done
--   In the recursive case, we want to start over with the SOP


-- Finally, by providing a map from SOP elements to String's,
--   we can parse/output Value's

-- We can either unsafely pull out names from annotations, then match up to the names
--   or we can try to extract the names from the parsed annotated version


-- deepToValue' :: forall instr t. SingI t => SOP (SomeTFValue' instr) (DeepTCode t) -> Value' instr t
-- deepToValue' = toValue' . fromDeep

-- deepFromValue' :: forall instr t. Sing t -> Value' instr t -> SOP (SomeTFValue' instr) (DeepTCode t)
-- deepFromValue' = toDeep . fromValue'


-- slistI2Value2TCode :: forall instr t. SingI t :- SListI2 (Value2' instr (TCode t))
-- slistI2Value2TCode = _

-- genericValue' :: forall instr t. SingI t :- Generic (Value' instr t)
-- genericValue' = _

-- fromOr ::  Sing l -> Sing r -> Either (Value' instr l) (Value' instr r) -> SOP I (Value2' instr (TCode l ++ TCode r))
-- fromOr sl sr xs = _ sl sr xs

-- fromPair :: Sing l -> Sing r -> Value' instr l -> Value' instr r -> SOP I (Value2' instr (TCode l ** TCode r))
-- fromPair sl sr xs ys = _ sl sr xs ys

-- instance HasDict (SingI a) (SList a) where
--   evidence SNil = Dict
--   evidence (SCons x xs) =
--     case evidence xs of
--       Dict -> Dict

-- toOr :: forall a b instr. Sing a -> Sing b -> SOP I (Value2' instr (TCode a ++ TCode b)) -> Value' instr ('TOr a b)
-- toOr sa sb xs = VOr $
--   case singTCode sa of
--     SCons sa' sas -> _ sa' sas sb xs
--     SNil -> Right $
--       withDict sb $
--       withDict (genericValue' @instr @b) $
--       to @(Value' instr b) xs

-- toPair :: Sing a -> Sing b -> SOP I (Value2' instr (TCode a ** TCode b)) -> Value' instr ('TPair a b)
-- toPair sa sb xs = undefined
--   -- _ (singTCode sa) (singTCode sb)
--   -- sa sb xs


-- instance (SingI t, SListI2 (Code (Value' instr t))) => Generic (Value' instr t) where
--   type Code (Value' instr t) = Value2' instr (TCode t)

--   Value' instr t -> SOP (ValueF' instr) (TCode t)

--   from :: Value' instr t -> SOP I (Value2' instr (TCode t))
--   from (VC xs) =
--     case sing @t of
--       STc ct -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ I $ VC xs
--   from (VKey xs) =
--     case sing @t of
--       STKey -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ I $ (VKey xs)
--   from (VUnit) =
--     case sing @t of
--       STUnit -> SOP $ SOP.Z $ SOP.Nil
--   from (VSignature xs) =
--     case sing @t of
--       STSignature -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ I $ (VSignature xs)
--   from (VChainId xs) =
--     case sing @t of
--       STChainId -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ I $ (VChainId xs)
--   from (VOption xs) =
--     case xs of
--       Nothing ->
--         SOP $ SOP.Z $ SOP.Nil
--       Just xs' ->
--         case sing @t of
--           STOption st ->
--             case from xs' of
--               SOP ys ->
--                 SOP $ SOP.S ys
--   from (VList xs) =
--     case sing @t of
--       (STList t) -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ I $ (VList xs)
--   from (VSet xs) =
--     case sing @t of
--       (STSet t) -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ I $ (VSet xs)
--   from (VOp xs) =
--     case sing @t of
--       STOperation -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ I $ (VOp xs)
--   from (VContract xs) =
--     case sing @t of
--       (STContract p) -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ I $ (VContract xs)
--   from (VPair (xs, ys)) =
--     case sing @t of
--       STPair sl sr ->
--         fromPair sl sr xs ys
--
--         -- _ sl sr (from @(Value' instr (FstTPair t)) xs) (from @(Value' instr (SndTPair t)) ys)
--         -- sl sr -- :: forall l r instr. (Value' instr l, Value' instr r) -> Value' instr ('TPair l r)
--
--   from (VOr xs) =
--     case sing @t of
--       STOr stl str ->
--         fromOr stl str xs
--
--         -- withDict (singTCode stl) $
--         -- withDict (singTCode str) $
--         -- -- withDict (slistI2Value2Append @instr @(TCode (FstTOr t)) @(TCode (SndTOr t))) $
--         -- case xs of
--         --   Left xs' -> undefined
--         --     -- _ stl str (from @(Value' instr (FstTOr t)) xs') -- (from @(Value' instr (FstTOr t)) xs')
--         --   Right xs' ->
--         --     case sing @(TCode (FstTOr t)) of
--         --       SNil -> from @(Value' instr (SndTOr t)) xs'
--         --       SCons a as -> _ a as stl str (from @(Value' instr (SndTOr t)) xs') -- (from @(Value' instr (FstTOr t)) xs')
--
--         -- case sing @(TCode (FstTOr t)) of
--         --   SNil ->
--         --     case xs of
--         --       Right xs' ->
--         --         from @(Value' instr (SndTOr t)) xs'
--         --   SCons a as ->
--         --     case xs of
--         --       Left xs' ->
--         --         _ (from @(Value' instr (FstTOr t)) xs') a as str
--         --       Right xs' -> _
--         --         -- _ (from @(Value' instr (SndTOr t)) xs') a as str
--
--         -- case xs of
--         --   Left xs' -> -- _ xs' stl str
--         --     case from @(Value' instr (FstTOr t)) xs' of
--         --       SOP ys -> SOP ys
--         --   Right xs' ->
--         --     case from @(Value' instr (SndTOr t)) xs' of
--         --       SOP ys -> SOP $ _ ys
--
--           -- _ xs' stl str
--             -- _ stl str (
--           -- (from @(Value' instr (SndTPair t)) ys)
--           -- (_ sl)
--           -- (_ sr)
--
--             -- case from xs' of
--             --   SOP ys -> SOP ys
--                 -- SOP $ ys:: forall l r instr. Either (Value' instr l) (Value' instr r) -> Value' instr ('TOr l r)
--
--   from (VLam xs) =
--     case sing @t of
--       (STLambda inp out) -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ I $ (VLam xs)
--   from (VMap xs) =
--     case sing @t of
--       (STMap k v) -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ I $ (VMap xs)
--   from (VBigMap xs) =
--     case sing @t of
--       (STBigMap k v) -> SOP $ SOP.Z $ (SOP.:* SOP.Nil) $ I $ (VBigMap xs)

--   to :: SOP I (Value2' instr (TCode t)) -> Value' instr t
--   to xs =
--     case sing @t of
--       STc ct ->
--         case xs of
--           SOP (SOP.Z ((SOP.:*) (I ys) SOP.Nil)) -> ys
--       STKey ->
--         case xs of
--           SOP (SOP.Z ((SOP.:*) (I ys) SOP.Nil)) -> ys
--       STUnit ->
--         case xs of
--           SOP (SOP.Z SOP.Nil) -> VUnit
--       STSignature ->
--         case xs of
--           SOP (SOP.Z ((SOP.:*) (I ys) SOP.Nil)) -> ys
--       STChainId ->
--         case xs of
--           SOP (SOP.Z ((SOP.:*) (I ys) SOP.Nil)) -> ys
--       STOption st ->
--         case xs of
--           SOP (SOP.Z SOP.Nil) -> VOption Nothing
--           SOP (SOP.S xs') -> VOption $ Just $
--             to @(Value' instr (UnTOption t)) (SOP xs')
--       STList t ->
--         case xs of
--           SOP (SOP.Z ((SOP.:*) (I ys) SOP.Nil)) -> ys
--       STSet ct ->
--         case xs of
--           SOP (SOP.Z ((SOP.:*) (I ys) SOP.Nil)) -> ys
--       STOperation ->
--         case xs of
--           SOP (SOP.Z ((SOP.:*) (I ys) SOP.Nil)) -> ys
--       STContract t ->
--         case xs of
--           SOP (SOP.Z ((SOP.:*) (I ys) SOP.Nil)) -> ys
--       STPair a b ->
--         toPair a b xs
--       STOr a b ->
--         toOr a b xs
--       STLambda a b ->
--         case xs of
--           SOP (SOP.Z ((SOP.:*) (I ys) SOP.Nil)) -> ys
--       STMap a b ->
--         case xs of
--           SOP (SOP.Z ((SOP.:*) (I ys) SOP.Nil)) -> ys
--       STBigMap a b ->
--         case xs of
--           SOP (SOP.Z ((SOP.:*) (I ys) SOP.Nil)) -> ys

-- type family UnTOption (t :: T) :: T where
--   UnTOption ('TOption t) = t
--
-- type family FstTPair (t :: T) :: T where
--   FstTPair ('TPair l _) = l
--
-- type family SndTPair (t :: T) :: T where
--   SndTPair ('TPair _ r) = r
--
-- type family FstTOr (t :: T) :: T where
--   FstTOr ('TOr l _) = l
--
-- type family SndTOr (t :: T) :: T where
--   SndTOr ('TOr _ r) = r




-- type family Value1' (instr :: [T] -> [T] -> Type) (xs :: [T]) :: [Type] where
--   Value1' instr '[] = '[]
--   Value1' instr (x ': xs) = Value' instr x ': Value1' instr xs
--
-- type family Value2' (instr :: [T] -> [T] -> Type) (xs :: [[T]]) :: [[Type]] where
--   Value2' instr '[] = '[]
--   Value2' instr (x ': xs) = Value1' instr x ': Value2' instr xs
--
-- slistI2Value2Append :: SListI2 (Value2' instr (xs ++ ys)) :- (SListI2 (Value2' instr xs), SListI2 (Value2' instr ys))
-- slistI2Value2Append = _
--
-- type Value2 (xs :: [[T]]) = Value2' Instr xs
--
--
-- data TF (t :: T) where
--   TFc :: Sing ct -> TF ('Tc ct)
--   TFKey :: TF ('TKey )
--   -- | TFUnit -- empty sum
--   TFSignature :: TF ('TSignature )
--   -- | TFOption (TF a) -- converted to SOP
--   TFList :: Sing a -> TF ('TList  a)
--   TFSet :: Sing a -> TF ('TSet  a)
--   TFOperation :: TF ('TOperation )
--   TFContract :: Sing a -> TF ('TContract  a)
--   -- | TFPair T T -- converted to SOP
--   -- | TFOr T T -- converted to SOP
--   TFLambda :: Sing a -> Sing b -> TF ('TLambda  a b)
--   TFMap :: Sing a -> Sing b -> TF ('TMap  a b)
--   TFBigMap :: Sing a -> Sing b -> TF ('TBigMap  a b)
--
-- data instance Sing :: TF t -> Type where
--   STFc :: Sing ('TFc sct)
--   STFKey :: Sing 'TFKey
--   -- | TFUnit -- empty sum
--   STFSignature :: Sing 'TFSignature
--   -- | TFOption (TF a) -- converted to SOP
--   STFList :: Sing ('TFList st)
--   STFSet :: Sing ('TFSet st)
--   STFOperation :: Sing 'TFOperation
--   STFContract :: Sing ('TFContract st)
--   -- | TFPair T T -- converted to SOP
--   -- | TFOr T T -- converted to SOP
--   STFLambda :: Sing ('TFLambda sa sb)
--   STFMap :: Sing ('TFMap sa sb)
--   STFBigMap :: Sing ('TFBigMap sa sb)
--
-- data ValueF' (f :: ([T] -> [T] -> Type) -> T -> Type) (instr :: [T] -> [T] -> Type) (tf :: TF t) where
--   VFC :: forall f instr ct (sct :: Sing ct). Sing sct -> Value' instr ('Tc ct) -> ValueF' f instr ('TFc sct)
--   VFKey :: forall f instr. Value' instr 'TKey -> ValueF' f instr 'TFKey
--   -- -- | TFUnit -- empty sum
--   VFSignature :: forall f instr. Value' instr 'TSignature -> ValueF' f instr 'TFSignature
--   -- -- | TFOption (TF a) -- converted to SOP
--   VFList :: forall f instr t (st :: Sing t). Sing st -> f instr t -> ValueF' f instr ('TFList st)
--   VFSet :: forall f instr t (st :: Sing t). Sing st -> Value' instr ('TSet t) -> ValueF' f instr ('TFSet st)
--   VFOperation :: forall f instr. Value' instr 'TOperation -> ValueF' f instr 'TFOperation
--   VFContract :: forall f instr t (st :: Sing t). Sing st -> Value' instr ('TContract t) -> ValueF' f instr ('TFContract st)
--   -- -- | TFPair T T -- converted to SOP
--   -- -- | TFOr T T -- converted to SOP
--   VFLam :: forall a b f instr (sa :: Sing a) (sb :: Sing b). Sing sa -> Sing sb -> Value' instr ('TLambda a b) -> ValueF' f instr ('TFLambda sa sb)
--   VFMap :: forall a b f instr (sa :: Sing a) (sb :: Sing b). Sing sa -> Sing sb -> Map (CValue a) (f instr b) -> ValueF' f instr ('TFMap sa sb)
--   VFBigMap :: forall a b f instr (sa :: Sing a) (sb :: Sing b). Sing sa -> Sing sb -> Map (CValue a) (f instr b) -> ValueF' f instr ('TFBigMap sa sb)
--
-- data SomeTF where
--   SomeTF :: forall t. TF t -> SomeTF
--
-- data SomeTFValue' (f :: ([T] -> [T] -> Type) -> T -> Type) (instr :: [T] -> [T] -> Type) (stf :: SomeTF) where
--   SomeTFValue' :: forall f instr tf. ValueF' f instr tf -> SomeTFValue' f instr ('SomeTF tf)
--
-- | The limit of `TCode`
-- type family DeepTCode (t :: T) :: [[SomeTF]] where
  -- DeepTCode ('Tc ct) = '[ '[ 'Tc ct]]
  -- DeepTCode ('TKey) = '[ '[ 'TKey]]
  -- DeepTCode ('TUnit) = '[ '[]]
  -- DeepTCode ('TSignature) = '[ '[ 'TSignature]]
  -- DeepTCode ('TChainId) = '[ '[ 'TChainId]]
  -- DeepTCode ('TOption t) = '[] ': DeepTCode t
  -- DeepTCode ('TList t) = '[ '[ 'TList t]]
  -- DeepTCode ('TSet ct) = '[ '[ 'TSet ct]]
  -- DeepTCode ('TOperation) = '[ '[ 'TOperation]]
  -- DeepTCode ('TContract t) = '[ '[ 'TContract t]]
  -- DeepTCode ('TPair a b) = DeepTCode a ** DeepTCode b -- cartesian product
  -- DeepTCode ('TOr a b) = DeepTCode a ++ DeepTCode b
  -- DeepTCode ('TLambda a b) = '[ '[ 'TLambda a b]]
  -- DeepTCode ('TMap a b) = '[ '[ 'TMap a b]]
  -- DeepTCode ('TBigMap a b) = '[ '[ 'TBigMap a b]]
--
-- singDeepTCode :: Sing t -> Sing (DeepTCode t)
-- singDeepTCode (STc ct) = sing -- '[ '[ 'Tc ct]]
-- singDeepTCode (STKey) = sing -- '[ '[ 'TKey]]
-- singDeepTCode (STUnit) = sing -- '[ '[]]
-- singDeepTCode (STSignature) = sing -- '[ '[ 'TSignature]]
-- singDeepTCode (STChainId) = sing -- '[ '[ 'TChainId]]
-- singDeepTCode (STOption t) =
--   SCons SNil (singDeepTCode t) -- '[] ': DeepTCode t
-- singDeepTCode (STList t) = sing -- '[ '[ 'TList t]]
-- singDeepTCode (STSet ct) = sing -- '[ '[ 'TSet ct]]
-- singDeepTCode (STOperation) = sing -- '[ '[ 'TOperation]]
-- singDeepTCode (STContract t) = sing -- '[ '[ 'TContract t]]
-- singDeepTCode (STPair a b) = singCart (singDeepTCode a) (singDeepTCode b) -- DeepTCode a ** DeepTCode b -- cartesian product
-- singDeepTCode (STOr a b) = singAppend (singDeepTCode a) (singDeepTCode b) -- DeepTCode a ++ DeepTCode b
-- singDeepTCode (STLambda a b) = sing -- '[ '[ 'TLambda a b]]
-- singDeepTCode (STMap a b) = sing -- '[ '[ 'TMap a b]]
-- singDeepTCode (STBigMap a b) = sing -- '[ '[ 'TBigMap a b]]
--
-- type instance Shallow = ShallowT
--   everything but VList, VMap, VBigMap
--
-- type instance StepDeep = StepDeepT
--   StepDeepT = if VList, VMap, VBigMap then Right, else Left
--
-- fromDeep :: forall f instr t. SingI t
--   => (forall t'. SingI t' => f instr t' -> Value' instr t')
--   -> SOP (SomeTFValue' f instr) (DeepTCode t)
--   -> SOP (Value' instr) (TCode t)
-- fromDeep = _
--
-- toDeep :: forall f instr t. SingI t
--   => (forall t'. SingI t' => Value' instr t' -> f instr t')
--   -> SOP (Value' instr) (TCode t)
--   -> SOP (SomeTFValue' f instr) (DeepTCode t)
-- toDeep = _
--
-- newtype ValueRec' (instr :: [T] -> [T] -> *) (t :: T) where
--   ValueRec' :: forall instr t.
--        SOP (SomeTFValue' ValueRec' instr) (DeepTCode t)
--     -> ValueRec' instr t
--
-- fromRecStep :: forall instr t. SingI t => ValueRec' instr t -> Value' instr t
-- fromRecStep (ValueRec' xs) =
--   toValue' $ fromDeep @ValueRec' @instr @t fromRecStep xs
--
-- fromRec :: forall instr t. SingI t => ValueRec' instr t -> SOP (Value' instr) (TCode t)
-- fromRec (ValueRec' xs) =
--   fromDeep @ValueRec' @instr @t fromRecStep xs
--
-- toRecStep :: forall instr t. SingI t => Value' instr t -> ValueRec' instr t
-- toRecStep xs =
--   toRec $ fromValue' xs
--
-- toRec :: forall instr t. SingI t => SOP (Value' instr) (TCode t) -> ValueRec' instr t
-- toRec xs =
--   ValueRec' $ toDeep @ValueRec' @instr @t toRecStep xs

