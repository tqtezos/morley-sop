{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-type-patterns #-}

module Michelson.Typed.Annotation.Sing.Alg where

import Data.Kind
import Data.String
import Prelude hiding (show)

import Text.Show

import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TH

import Michelson.Typed.T (CT(..))
import Michelson.Typed.T.Alg
import Data.Constraint.HasDict1

import Michelson.Typed.Annotation.Sing (Annotated)
import Michelson.Typed.Annotation.Sing.Notes ()
import Michelson.Typed.Annotation.Sing.Opq
import qualified Michelson.Typed.Annotation.Sing as Michelson

-- | The "algebraic", i.e. or/pair, part of `Annotated`
data AnnotatedAlg a (t :: TAlg) where
  ATPair      :: a -> a -> a
              -> AnnotatedAlg a p -> AnnotatedAlg a q -> AnnotatedAlg a ('TPair p q)
  ATOr        :: a -> a -> a
              -> AnnotatedAlg a p -> AnnotatedAlg a q -> AnnotatedAlg a ('TOr p q)

  ATOpq       :: AnnotatedOpq a t -> AnnotatedAlg a ('TOpq t)

$(genPromotions [''AnnotatedAlg])

$(singletons [d|
  instance Show a => Show (AnnotatedAlg a t) where
    showsPrec d (ATPair x1 x2 x3 x4 x5) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATPair" d (x1, x2, x3, x4, x5)
    showsPrec d (ATOr x1 x2 x3 x4 x5) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATOr" d (x1, x2, x3, x4, x5)
    showsPrec d (ATOpq x1) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATOpq" d x1

  traverseAnnotatedAlg :: Applicative f => (a -> f b) -> AnnotatedAlg a t -> f (AnnotatedAlg b t)
  traverseAnnotatedAlg fs (ATPair x1 x2 x3 x4 x5) = ATPair <$> fs x1 <*> fs x2 <*> fs x3 <*> traverseAnnotatedAlg fs x4 <*> traverseAnnotatedAlg fs x5
  traverseAnnotatedAlg fs (ATOr x1 x2 x3 x4 x5) = ATOr <$> fs x1 <*> fs x2 <*> fs x3 <*> traverseAnnotatedAlg fs x4 <*> traverseAnnotatedAlg fs x5
  traverseAnnotatedAlg fs (ATOpq x1) = ATOpq <$> traverseAnnotatedOpq fs x1

  |])

data instance Sing :: AnnotatedAlg a t -> Type where
  SATPair      :: forall a s t (ta :: a) (tb :: a) (tc :: a) (xs :: AnnotatedAlg a s) (ys :: AnnotatedAlg a t). Sing ta -> Sing tb -> Sing tc -> Sing xs -> Sing ys -> Sing ('ATPair ta tb tc xs ys)
  SATOr        :: forall a s t (ta :: a) (tb :: a) (tc :: a) (xs :: AnnotatedAlg a s) (ys :: AnnotatedAlg a t). Sing ta -> Sing tb -> Sing tc -> Sing xs -> Sing ys -> Sing ('ATOr ta tb tc xs ys)

  SATOpq       :: forall a t (xs :: AnnotatedOpq a t). Sing xs -> Sing ('ATOpq xs)

instance (SingI ta,  SingI tb,  SingI tc,  SingI xs,  SingI ys) => SingI ('ATPair ta tb tc xs ys) where
  sing = SATPair sing sing sing sing sing
instance (SingI ta,  SingI tb,  SingI tc,  SingI xs,  SingI ys) => SingI ('ATOr ta tb tc xs ys) where
  sing = SATOr sing sing sing sing sing
instance (SingI xs) => SingI ('ATOpq xs) where
  sing = SATOpq sing


instance SingKind a => SingKind (AnnotatedAlg a t) where
  type Demote (AnnotatedAlg a t) = AnnotatedAlg (Demote a) t

  fromSing (SATPair ta tb tc xs ys) = ATPair (fromSing ta) (fromSing tb) (fromSing tc) (fromSing xs) (fromSing ys)
  fromSing (SATOr ta tb tc xs ys) = ATOr (fromSing ta) (fromSing tb) (fromSing tc) (fromSing xs) (fromSing ys)
  fromSing (SATOpq xs) = ATOpq (fromSing xs)

  toSing (ATPair ta tb tc xs ys) =
    case (toSing ta, toSing tb, toSing tc, toSing xs, toSing ys) of
      (SomeSing sta, SomeSing stb, SomeSing stc, SomeSing sxs, SomeSing sys) ->
        SomeSing $
        SATPair sta stb stc sxs sys
  toSing (ATOr ta tb tc xs ys) =
    case (toSing ta, toSing tb, toSing tc, toSing xs, toSing ys) of
      (SomeSing sta, SomeSing stb, SomeSing stc, SomeSing sxs, SomeSing sys) ->
        SomeSing $
        SATOr sta stb stc sxs sys
  toSing (ATOpq xs) =
    case toSing xs of
      SomeSing sxs ->
        SomeSing $
          SATOpq sxs

instance HasDict1 a => HasDict1 (AnnotatedAlg a t) where
  evidence1 = $(gen_evidence1 ''AnnotatedAlg)


type ExampleTAlg
   = 'TPair ('TOpq ('Tc 'CNat)) ('TPair ('TOpq ('Tc 'CAddress)) ('TOpq ('Tc 'CNat)))

type ExampleTAnn
   = 'ATPair "" "balance" "request" ('ATOpq ('ATc "")) ('ATPair "" "owner" "token_id" ('ATOpq ('ATc "")) ('ATOpq ('ATc "")))

---- | `sFieldToTypeAnn` applied to `ExampleTAnn`:
----
---- @
----  exampleFieldToTypeAnn = show . fromSing $ `sFieldToTypeAnn` @Symbol @`ExampleTAlg` (sing @`ExampleTAnn`)
---- @
--exampleFieldToTypeAnn :: String
--exampleFieldToTypeAnn = show . fromSing $ sFieldToTypeAnn @Symbol @ExampleTAlg (sing @ExampleTAnn)

type family ToAnnotatedAlg (ann :: Annotated a t) :: AnnotatedAlg a (ToTAlg t) where
  ToAnnotatedAlg ('Michelson.ATc ta) = 'ATOpq ('ATc ta)
  ToAnnotatedAlg ('Michelson.ATKey ta) = 'ATOpq ('ATKey ta)
  ToAnnotatedAlg ('Michelson.ATUnit ta) = 'ATOpq ('ATUnit ta)
  ToAnnotatedAlg ('Michelson.ATSignature ta) = 'ATOpq ('ATSignature ta)
  ToAnnotatedAlg ('Michelson.ATChainId ta) = 'ATOpq ('ATChainId ta)
  ToAnnotatedAlg ('Michelson.ATOption ta xs) = 'ATOpq ('ATOption ta xs)
  ToAnnotatedAlg ('Michelson.ATList ta xs) = 'ATOpq ('ATList ta xs)
  ToAnnotatedAlg ('Michelson.ATSet ta tb) = 'ATOpq ('ATSet ta tb)
  ToAnnotatedAlg ('Michelson.ATOperation ta) = 'ATOpq ('ATOperation ta)
  ToAnnotatedAlg ('Michelson.ATContract ta xs) = 'ATOpq ('ATContract ta xs)
  ToAnnotatedAlg ('Michelson.ATPair ta tb tc xs ys) = ('ATPair ta tb tc (ToAnnotatedAlg xs) (ToAnnotatedAlg ys))
  ToAnnotatedAlg ('Michelson.ATOr ta tb tc xs ys) = ('ATOr ta tb tc (ToAnnotatedAlg xs) (ToAnnotatedAlg ys))
  ToAnnotatedAlg ('Michelson.ATLambda ta xs ys) = 'ATOpq ('ATLambda ta xs ys)
  ToAnnotatedAlg ('Michelson.ATMap ta tb xs) = 'ATOpq ('ATMap ta tb xs)
  ToAnnotatedAlg ('Michelson.ATBigMap ta tb xs) = 'ATOpq ('ATBigMap ta tb xs)

-- | Convert a `Sing` `Annotated` to an `AnnotatedAlg`
singToAnnotatedAlg :: forall a t (ann :: Annotated a t). Sing ann -> Sing (ToAnnotatedAlg ann)
singToAnnotatedAlg (Michelson.SATc ta) = SATOpq (SATc ta)
singToAnnotatedAlg (Michelson.SATKey ta) = SATOpq (SATKey ta)
singToAnnotatedAlg (Michelson.SATUnit ta) = SATOpq (SATUnit ta)
singToAnnotatedAlg (Michelson.SATSignature ta) = SATOpq (SATSignature ta)
singToAnnotatedAlg (Michelson.SATChainId ta) = SATOpq (SATChainId ta)
singToAnnotatedAlg (Michelson.SATOption ta xs) = SATOpq (SATOption ta xs)
singToAnnotatedAlg (Michelson.SATList ta xs) = SATOpq (SATList ta xs)
singToAnnotatedAlg (Michelson.SATSet ta tb) = SATOpq (SATSet ta tb)
singToAnnotatedAlg (Michelson.SATOperation ta) = SATOpq (SATOperation ta)
singToAnnotatedAlg (Michelson.SATContract ta xs) = SATOpq (SATContract ta xs)
singToAnnotatedAlg (Michelson.SATPair ta tb tc xs ys) = (SATPair ta tb tc (singToAnnotatedAlg xs) (singToAnnotatedAlg ys))
singToAnnotatedAlg (Michelson.SATOr ta tb tc xs ys) = (SATOr ta tb tc (singToAnnotatedAlg xs) (singToAnnotatedAlg ys))
singToAnnotatedAlg (Michelson.SATLambda ta xs ys) = SATOpq (SATLambda ta xs ys)
singToAnnotatedAlg (Michelson.SATMap ta tb xs) = SATOpq (SATMap ta tb xs)
singToAnnotatedAlg (Michelson.SATBigMap ta tb xs) = SATOpq (SATBigMap ta tb xs)

type family FromAnnotatedAlg (ann :: AnnotatedAlg a t) :: Annotated a (FromTAlg t) where
  FromAnnotatedAlg ('ATOpq ('ATc ta)) = ('Michelson.ATc ta)
  FromAnnotatedAlg ('ATOpq ('ATKey ta)) = ('Michelson.ATKey ta)
  FromAnnotatedAlg ('ATOpq ('ATUnit ta)) = ('Michelson.ATUnit ta)
  FromAnnotatedAlg ('ATOpq ('ATSignature ta)) = ('Michelson.ATSignature ta)
  FromAnnotatedAlg ('ATOpq ('ATChainId ta)) = ('Michelson.ATChainId ta)
  FromAnnotatedAlg ('ATOpq ('ATOption ta xs)) = ('Michelson.ATOption ta xs)
  FromAnnotatedAlg ('ATOpq ('ATList ta xs)) = ('Michelson.ATList ta xs)
  FromAnnotatedAlg ('ATOpq ('ATSet ta tb)) = ('Michelson.ATSet ta tb)
  FromAnnotatedAlg ('ATOpq ('ATOperation ta)) = ('Michelson.ATOperation ta)
  FromAnnotatedAlg ('ATOpq ('ATContract ta xs)) = ('Michelson.ATContract ta xs)
  FromAnnotatedAlg ('ATOpq ('ATLambda ta xs ys)) = ('Michelson.ATLambda ta xs ys)
  FromAnnotatedAlg ('ATOpq ('ATMap ta tb xs)) = ('Michelson.ATMap ta tb xs)
  FromAnnotatedAlg ('ATOpq ('ATBigMap ta tb xs)) = ('Michelson.ATBigMap ta tb xs)
  FromAnnotatedAlg ('ATPair ta tb tc xs ys) = ('Michelson.ATPair ta tb tc (FromAnnotatedAlg xs) (FromAnnotatedAlg ys))
  FromAnnotatedAlg ('ATOr ta tb tc xs ys) = ('Michelson.ATOr ta tb tc (FromAnnotatedAlg xs) (FromAnnotatedAlg ys))

-- | Convert a `Sing` `AnnotatedAlg` to an `Annotated`
singFromAnnotatedAlg :: forall a t (ann :: AnnotatedAlg a t). Sing ann -> Sing (FromAnnotatedAlg ann)
singFromAnnotatedAlg (SATOpq (SATc ta)) = (Michelson.SATc ta)
singFromAnnotatedAlg (SATOpq (SATKey ta)) = (Michelson.SATKey ta)
singFromAnnotatedAlg (SATOpq (SATUnit ta)) = (Michelson.SATUnit ta)
singFromAnnotatedAlg (SATOpq (SATSignature ta)) = (Michelson.SATSignature ta)
singFromAnnotatedAlg (SATOpq (SATChainId ta)) = (Michelson.SATChainId ta)
singFromAnnotatedAlg (SATOpq (SATOption ta xs)) = (Michelson.SATOption ta xs)
singFromAnnotatedAlg (SATOpq (SATList ta xs)) = (Michelson.SATList ta xs)
singFromAnnotatedAlg (SATOpq (SATSet ta tb)) = (Michelson.SATSet ta tb)
singFromAnnotatedAlg (SATOpq (SATOperation ta)) = (Michelson.SATOperation ta)
singFromAnnotatedAlg (SATOpq (SATContract ta xs)) = (Michelson.SATContract ta xs)
singFromAnnotatedAlg (SATOpq (SATLambda ta xs ys)) = (Michelson.SATLambda ta xs ys)
singFromAnnotatedAlg (SATOpq (SATMap ta tb xs)) = (Michelson.SATMap ta tb xs)
singFromAnnotatedAlg (SATOpq (SATBigMap ta tb xs)) = (Michelson.SATBigMap ta tb xs)
singFromAnnotatedAlg (SATPair ta tb tc xs ys) = (Michelson.SATPair ta tb tc (singFromAnnotatedAlg xs) (singFromAnnotatedAlg ys))
singFromAnnotatedAlg (SATOr ta tb tc xs ys) = (Michelson.SATOr ta tb tc (singFromAnnotatedAlg xs) (singFromAnnotatedAlg ys))

-- | Uses `singFromAnnotatedAlg`
instance Eq (AnnotatedAlg Text t) where
  xs == ys =
    case (toSing xs, toSing ys) of
      (SomeSing sxs, SomeSing sys) ->
        fromSing (singFromAnnotatedAlg sxs) == fromSing (singFromAnnotatedAlg sys)

