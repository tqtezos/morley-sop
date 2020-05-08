-- {-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.Annotation.Sing.Scope where

-- import Data.Kind
-- import Data.Type.Equality
-- import Data.Void
-- import GHC.TypeLits

-- import Lorentz
-- import Michelson.Typed.Annotation
-- import Michelson.Untyped.Annotation
-- import Michelson.Typed.T
-- import Michelson.Typed.Sing
-- import Lorentz.EntryPoints.Core

-- import Data.Singletons
-- import Data.Singletons.TH
-- import Data.Singletons.TypeLits
-- import Data.Constraint
-- -- import Fcf (Exp)
-- -- import qualified Fcf as Fcf

-- import Michelson.Typed.Annotation.Sing
-- import Michelson.Typed.T.Scope
-- import Data.Singletons.WrappedSing


-- tt = _

-- class (forall (c :: Constraint). c) => VoidC
-- instance (forall (c :: Constraint). c) => VoidC

-- toVoidC :: forall c. (Dict c -> Void) -> c :- VoidC
-- toVoidC f = Sub $ absurd $ f (Dict @c)

-- -- | If `VoidC`, we can derive any `Constraint`
-- absurdC :: forall c. VoidC :- c
-- absurdC = Sub Dict

-- -- | `VoidC` entails `Void`
-- --
-- -- Use `absurdC` to derive @`Monoid` `Void`@.
-- voidC :: VoidC => Void
-- voidC =
--   case absurdC @(Monoid Void) `mapDict` Dict of
--     Dict -> mempty





-- type family NoOr (t :: T) :: Constraint where
--   NoOr ('Tc _) = ()
--   NoOr 'TKey = ()
--   NoOr 'TUnit = ()
--   NoOr 'TSignature = ()
--   NoOr 'TChainId = ()
--   NoOr ('TOption _) = ()
--   NoOr ('TList _) = ()
--   NoOr ('TSet _) = ()
--   NoOr 'TOperation = ()
--   NoOr ('TContract _) = ()
--   NoOr ('TPair _ _) = ()
--   NoOr ('TOr _ _) = VoidC
--   NoOr ('TLambda _ _) = ()
--   NoOr ('TMap _ _) = ()
--   NoOr ('TBigMap _ _) = ()

-- -- noOr :: Sing a -> Sing b -> Dict (NoOr ('TOr a b)) -> Void
-- -- noOr sa sb = \case

-- -- data instance Sing :: NoOr t -> Type where
-- --   SNoOr :: NoOr t => Sing (NoOr t)

-- -- data NoOr' (t :: T) where
-- --   NoOr' :: (HasOr t -> Void) -> NoOr'

-- data HasOr (t :: T) where
--   HasOr :: Sing a -> Sing b -> HasOr ('TOr a b)

-- hasOr :: Sing t -> Either (Dict (NoOr t)) (HasOr t)
-- hasOr =
--   \case
--     STc _ -> Left Dict
--     STPair _ _ -> Left Dict
--     STKey -> Left Dict
--     STUnit -> Left Dict
--     STSignature -> Left Dict
--     STChainId -> Left Dict
--     STOption _ -> Left Dict
--     STList _ -> Left Dict
--     STSet _ -> Left Dict
--     STOperation -> Left Dict
--     STContract _ -> Left Dict
--     STOr sa sb -> Right $ HasOr sa sb
--     STLambda _ _ -> Left Dict
--     STMap _ _ -> Left Dict
--     STBigMap _ _ -> Left Dict

-- type family NoPair (t :: T) :: Constraint where
--   NoPair ('Tc _) = ()
--   NoPair 'TKey = ()
--   NoPair 'TUnit = ()
--   NoPair 'TSignature = ()
--   NoPair 'TChainId = ()
--   NoPair ('TOption _) = ()
--   NoPair ('TList _) = ()
--   NoPair ('TSet _) = ()
--   NoPair 'TOperation = ()
--   NoPair ('TContract _) = ()
--   NoPair ('TPair _ _) = VoidC
--   NoPair ('TOr _ _) = ()
--   NoPair ('TLambda _ _) = ()
--   NoPair ('TMap _ _) = ()
--   NoPair ('TBigMap _ _) = ()

-- data HasPair (t :: T) where
--   HasPair :: Sing a -> Sing b -> HasPair ('TPair a b)

-- hasPair :: Sing t -> Either (Dict (NoPair t)) (HasPair t)
-- hasPair =
--   \case
--     STc _ -> Left Dict
--     STPair sa sb -> Right $ HasPair sa sb
--     STKey -> Left Dict
--     STUnit -> Left Dict
--     STSignature -> Left Dict
--     STChainId -> Left Dict
--     STOption _ -> Left Dict
--     STList _ -> Left Dict
--     STSet _ -> Left Dict
--     STOperation -> Left Dict
--     STContract _ -> Left Dict
--     STOr _ _ -> Left Dict
--     STLambda _ _ -> Left Dict
--     STMap _ _ -> Left Dict
--     STBigMap _ _ -> Left Dict


-- type family FieldAnnotation (ann :: Annotated a t) :: a where
--   FieldAnnotation ('ATc ta) = ta
--   FieldAnnotation ('ATKey ta) = ta
--   FieldAnnotation ('ATUnit ta) = ta
--   FieldAnnotation ('ATSignature ta) = ta
--   FieldAnnotation ('ATChainId ta) = ta
--   FieldAnnotation ('ATOption ta _) = ta
--   FieldAnnotation ('ATList ta _) = ta
--   FieldAnnotation ('ATSet ta _) = ta
--   FieldAnnotation ('ATOperation ta) = ta
--   FieldAnnotation ('ATContract ta _) = ta

--   FieldAnnotation ('ATPair ta tb tc xs ys) = TypeError (
--     'Text "FieldAnnotation ('ATPair " ':<>:
--     'ShowType ta ':<>:
--     'ShowType tb ':<>:
--     'ShowType tc ':<>:
--     'ShowType xs ':<>:
--     'ShowType ys ':<>:
--     'Text ")"
--     )
--   FieldAnnotation ('ATOr ta tb tc xs ys) = TypeError (
--     'Text "FieldAnnotation ('ATOr " ':<>:
--     'ShowType ta ':<>:
--     'ShowType tb ':<>:
--     'ShowType tc ':<>:
--     'ShowType xs ':<>:
--     'ShowType ys ':<>:
--     'Text ")"
--     )

--   FieldAnnotation ('ATLambda ta _ _) = ta
--   FieldAnnotation ('ATMap ta _ _) = ta
--   FieldAnnotation ('ATBigMap ta _ _) = ta


-- singFieldAnnotation :: forall a t (ann :: Annotated a t). (NoOr t, NoPair t) => Sing ann -> Sing (FieldAnnotation ann)
-- singFieldAnnotation (SATc ta) = ta
-- singFieldAnnotation (SATKey ta) = ta
-- singFieldAnnotation (SATUnit ta) = ta
-- singFieldAnnotation (SATSignature ta) = ta
-- singFieldAnnotation (SATChainId ta) = ta
-- singFieldAnnotation (SATOption ta _) = ta
-- singFieldAnnotation (SATList ta _) = ta
-- singFieldAnnotation (SATSet ta _) = ta
-- singFieldAnnotation (SATOperation ta) = ta
-- singFieldAnnotation (SATContract ta _) = ta
-- singFieldAnnotation (SATPair _ _ _ _ _) = absurd voidC
-- singFieldAnnotation (SATOr _ _ _ _ _) = absurd voidC
-- singFieldAnnotation (SATLambda ta _ _) = ta
-- singFieldAnnotation (SATMap ta _ _) = ta
-- singFieldAnnotation (SATBigMap ta _ _) = ta

-- -- | Requires `NoOr`
-- type family AnnotatedField (ann :: Annotated a t) (field :: a) :: Maybe T where
--   AnnotatedField ('ATc ta :: Annotated a t) ta = 'Just t
--   AnnotatedField ('ATc _) _ = 'Nothing

--   AnnotatedField ('ATPair _ ta tb xs ys) ta = 'Just x -- && Assert (IsNothing (AnnotatedField xs ta) && IsNothing (AnnotatedField ys ta))
--   AnnotatedField ('ATPair _ ta tb xs ys) tb = 'Just y -- && ..
--   AnnotatedField ('ATPair _ ta tb xs ys) field = AnnotatedField xs field <|> AnnotatedField ys field

--   AnnotatedField ('ATOr ta tb tc xs ys) = TypeError ..

-- annotatedToNotes :: Annotated Text t -> Notes t
-- annotatedToNotes (ATc ta) = NTc (AnnotationUnsafe ta)
-- annotatedToNotes (ATKey ta) = NTKey (AnnotationUnsafe ta)
-- annotatedToNotes (ATUnit ta) = NTUnit (AnnotationUnsafe ta)
-- annotatedToNotes (ATSignature ta) = NTSignature (AnnotationUnsafe ta)
-- annotatedToNotes (ATChainId ta) = NTChainId (AnnotationUnsafe ta)
-- annotatedToNotes (ATOption ta xs) = NTOption (AnnotationUnsafe ta) (annotatedToNotes xs)
-- annotatedToNotes (ATList ta xs) = NTList (AnnotationUnsafe ta) (annotatedToNotes xs)
-- annotatedToNotes (ATSet ta tb) = NTSet (AnnotationUnsafe ta) (AnnotationUnsafe tb)
-- annotatedToNotes (ATOperation ta) = NTOperation (AnnotationUnsafe ta)
-- annotatedToNotes (ATContract ta xs) = NTContract (AnnotationUnsafe ta) (annotatedToNotes xs)
-- annotatedToNotes (ATPair ta tb tc xs ys) = NTPair (AnnotationUnsafe ta) (AnnotationUnsafe tb) (AnnotationUnsafe tc) (annotatedToNotes xs) (annotatedToNotes ys)
-- annotatedToNotes (ATOr ta tb tc xs ys) = NTOr (AnnotationUnsafe ta) (AnnotationUnsafe tb) (AnnotationUnsafe tc) (annotatedToNotes xs) (annotatedToNotes ys)
-- annotatedToNotes (ATLambda ta xs ys) = NTLambda (AnnotationUnsafe ta) (annotatedToNotes xs) (annotatedToNotes ys)
-- annotatedToNotes (ATMap ta tb xs) = NTMap (AnnotationUnsafe ta) (AnnotationUnsafe tb) (annotatedToNotes xs)
-- annotatedToNotes (ATBigMap ta tb xs) = NTBigMap (AnnotationUnsafe ta) (AnnotationUnsafe tb) (annotatedToNotes xs)


-- class (NoOr t, NoPair t) => Reduced t
-- instance (NoOr t, NoPair t) => Reduced t

-- isReduced :: Sing t -> Either (NotReduced t) (Dict (IsReduced t))
-- isReduced = _

-- type family ReducedAnnotation (t :: T) (ann :: Annotated a t) :: a where
--   ReducedAnnotation ('ATc x) = x

-- reducedAnnotation :: forall a t (ann :: Annotated a t). IsReduced t
--   => Sing t
--   -> Sing ann
--   ->



-- data T
--   = Tc !CT
--   | TKey
--   | TUnit
--   | TSignature
--   | TChainId
--   | TOption !T
--   | TList !T
--   | TSet !CT
--   | TOperation
--   | TContract !T
--   | TPair !T !T
--   | TOr !T !T
--   | TLambda !T !T
--   | TMap !CT !T
--   | TBigMap !CT !T

-- class (forall a b. t ~ 'TOr a b => VoidC) => NoOr (t :: T)
-- instance (forall a b. t ~ 'TOr a b => VoidC) => NoOr (t :: T)

-- hasOr :: Sing t -> Either (Dict (NoOr t)) (HasOr t)
-- hasOr = undefined -- _

-- class (forall a b. (SingI a, SingI b, t ~ 'TPair a b) => VoidC) => NoPair (t :: T)
-- instance (forall a b. (SingI a, SingI b, t ~ 'TPair a b) => VoidC) => NoPair (t :: T)

-- toNoPair :: forall t. (forall a b. Dict (SingI a, SingI b, t ~ 'TPair a b) -> Void) -> Dict (NoPair t)
-- toNoPair f = _ f -- $ toVoidC @(t ~ 'TPair a b)

-- foo :: Dict (NoPair 'TKey)
-- foo = Dict

-- bar :: ('TKey ~ 'TUnit) :- VoidC
-- bar = Sub Dict

-- dictNoPair :: forall t. (forall a b. Sing a -> Sing b -> t :~: 'TPair a b -> Void) -> Dict (NoPair t)
-- dictNoPair f = _

-- data T
--   = Tc !CT
--   | TKey
--   | TUnit
--   | TSignature
--   | TChainId
--   | TOption !T
--   | TList !T
--   | TSet !CT
--   | TOperation
--   | TContract !T
--   | TPair !T !T
--   | TOr !T !T
--   | TLambda !T !T
--   | TMap !CT !T
--   | TBigMap !CT !T



