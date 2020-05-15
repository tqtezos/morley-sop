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
import Data.Singletons.Prelude.Applicative
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.IsString
import Data.Singletons.TH
import Data.Constraint

import Michelson.Typed.T (CT(..))
import Michelson.Typed.T.Alg
import Data.Constraint.HasDict1

import Data.Singletons.Prelude.Monad.State
import Michelson.Typed.Annotation.Sing (Annotated)
import Michelson.Typed.Annotation.Sing.Opq
import qualified Michelson.Typed.Annotation.Sing as Michelson


-- tt = _

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

  traverseAnnotatedAlg :: (a -> State' s b) -> AnnotatedAlg a t -> State' s (AnnotatedAlg b t)
  traverseAnnotatedAlg fs (ATPair x1 x2 x3 x4 x5) = ATPair <$$> fs x1 <<*>> fs x2 <<*>> fs x3 <<*>> traverseAnnotatedAlg fs x4 <<*>> traverseAnnotatedAlg fs x5
  traverseAnnotatedAlg fs (ATOr x1 x2 x3 x4 x5) = ATOr <$$> fs x1 <<*>> fs x2 <<*>> fs x3 <<*>> traverseAnnotatedAlg fs x4 <<*>> traverseAnnotatedAlg fs x5
  traverseAnnotatedAlg fs (ATOpq x1) = ATOpq <$$> traverseAnnotatedOpq fs x1

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


-- | A proof that `Sing` implies `SingI` for `AnnotatedAlg`, if it does for @a@
singIAnnotatedAlg :: forall a t (xs :: AnnotatedAlg a t). (forall (x :: a). Sing x -> Dict (SingI x)) -> Sing xs -> Dict (SingI xs)
singIAnnotatedAlg singIA (SATPair ta tb tc xs ys) =
  case (singIA ta, singIA tb, singIA tc, singIAnnotatedAlg singIA xs, singIAnnotatedAlg singIA ys) of
    (Dict, Dict, Dict, Dict, Dict) -> Dict
singIAnnotatedAlg singIA (SATOr ta tb tc xs ys) =
  case (singIA ta, singIA tb, singIA tc, singIAnnotatedAlg singIA xs, singIAnnotatedAlg singIA ys) of
    (Dict, Dict, Dict, Dict, Dict) -> Dict
singIAnnotatedAlg singIA (SATOpq xs) =
  case (singIAnnotatedOpq singIA xs) of
    (Dict) -> Dict

instance HasDict1 a => HasDict1 (AnnotatedAlg a t) where
  evidence1 = singIAnnotatedAlg evidence1


$(singletonsOnly [d|

  -- `TOpq` always has a `TypeAnn`
  tOpqTypeAnn :: forall a t. AnnotatedOpq a t -> a
  tOpqTypeAnn (ATc ta) = ta
  tOpqTypeAnn (ATKey ta) = ta
  tOpqTypeAnn (ATUnit ta) = ta
  tOpqTypeAnn (ATSignature ta) = ta
  tOpqTypeAnn (ATChainId ta) = ta
  tOpqTypeAnn (ATOption ta _) = ta
  tOpqTypeAnn (ATList ta _) = ta
  tOpqTypeAnn (ATSet ta _) = ta
  tOpqTypeAnn (ATOperation ta) = ta
  tOpqTypeAnn (ATContract ta _) = ta
  tOpqTypeAnn (ATLambda ta _ _) = ta
  tOpqTypeAnn (ATMap ta _ _) = ta
  tOpqTypeAnn (ATBigMap ta _ _) = ta

  -- `TOpq` always has a `TypeAnn`: set it
  setTypeAnn :: forall a t. a -> AnnotatedOpq a t -> AnnotatedOpq a t
  setTypeAnn as (ATc _ta) = ATc as
  setTypeAnn as (ATKey _ta) = ATKey as
  setTypeAnn as (ATUnit _ta) = ATUnit as
  setTypeAnn as (ATSignature _ta) = ATSignature as
  setTypeAnn as (ATChainId _ta) = ATChainId as
  setTypeAnn as (ATOption _ta xs) = ATOption as xs
  setTypeAnn as (ATList _ta xs) = ATList as xs
  setTypeAnn as (ATSet _ta xs) = ATSet as xs
  setTypeAnn as (ATOperation _ta) = ATOperation as
  setTypeAnn as (ATContract _ta xs) = ATContract as xs
  setTypeAnn as (ATLambda _ta xs ys) = ATLambda as xs ys
  setTypeAnn as (ATMap _ta xs ys) = ATMap as xs ys
  setTypeAnn as (ATBigMap _ta xs ys) = ATBigMap as xs ys

  propagateTypeAnn ::
       forall a t. (Eq a, IsString a)
    => a
    -> AnnotatedAlg a t
    -> AnnotatedAlg a t
  propagateTypeAnn as (ATPair ta tb tc xs ys) =
    bool_
      (fieldToTypeAnn (ATPair as tb tc xs ys))
      (fieldToTypeAnn (ATPair ta tb tc xs ys))
      (as == "")
  propagateTypeAnn _as (ATOr ta tb tc xs ys) =
       fieldToTypeAnn (ATOr ta tb tc xs ys)
  propagateTypeAnn as (ATOpq xs) =
    bool_ (ATOpq (setTypeAnn as xs)) (ATOpq xs) (as == "")

  fieldToTypeAnn :: forall a t. (Eq a, IsString a) => AnnotatedAlg a t -> AnnotatedAlg a t
  fieldToTypeAnn (ATPair ta tb tc xs ys) = ATPair ta tb tc (propagateTypeAnn tb xs) (propagateTypeAnn tc ys)
  fieldToTypeAnn (ATOr ta tb tc xs ys) = ATOr ta tb tc (fieldToTypeAnn xs) (fieldToTypeAnn ys) -- (propagateTypeAnn tb xs) (propagateTypeAnn tc ys)
  fieldToTypeAnn (ATOpq xs) = ATOpq xs



  |])



type ExampleTAlg
   = 'TPair ('TOpq ('Tc 'CNat)) ('TPair ('TOpq ('Tc 'CAddress)) ('TOpq ('Tc 'CNat)))

type ExampleTAnn
   = 'ATPair "" "balance" "request" ('ATOpq ('ATc "")) ('ATPair "" "owner" "token_id" ('ATOpq ('ATc "")) ('ATOpq ('ATc "")))

-- | `sFieldToTypeAnn` applied to `ExampleTAnn`:
--
-- @
--  exampleFieldToTypeAnn = show . fromSing $ `sFieldToTypeAnn` @Symbol @`ExampleTAlg` (sing @`ExampleTAnn`)
-- @
exampleFieldToTypeAnn :: String
exampleFieldToTypeAnn = show . fromSing $ sFieldToTypeAnn @Symbol @ExampleTAlg (sing @ExampleTAnn)

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

