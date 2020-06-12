{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-type-patterns #-}

module Michelson.Typed.Annotation.Sing.Opq where

import Data.Kind
import Data.String
import Prelude hiding (show)

import Text.Show

import Data.Singletons
import Data.Singletons.TH

import Michelson.Typed.T.Alg
import Data.Constraint.HasDict1

import Michelson.Typed.Annotation.Sing (Annotated, traverseAnnotated)

-- | `Annotated` without the "algebraic" part, i.e. or/pair
data AnnotatedOpq a (t :: TOpq) where
  ATc         :: a -> AnnotatedOpq a ('Tc ct)
  ATKey       :: a -> AnnotatedOpq a 'TKey
  ATUnit      :: a -> AnnotatedOpq a 'TUnit
  ATSignature :: a -> AnnotatedOpq a 'TSignature
  ATChainId   :: a -> AnnotatedOpq a 'TChainId
  ATOption    :: a -> Annotated a t -> AnnotatedOpq a ('TOption t)
  ATList      :: a -> Annotated a t -> AnnotatedOpq a ('TList t)
  ATSet       :: a -> a -> AnnotatedOpq a ('TSet ct)
  ATOperation :: a -> AnnotatedOpq a 'TOperation
  ATContract  :: a -> Annotated a t -> AnnotatedOpq a ('TContract t)
  ATLambda    :: a -> Annotated a p -> Annotated a q -> AnnotatedOpq a ('TLambda p q)
  ATMap       :: a -> a -> Annotated a v -> AnnotatedOpq a ('TMap k v)
  ATBigMap    :: a -> a -> Annotated a v -> AnnotatedOpq a ('TBigMap k v)

data instance Sing :: AnnotatedOpq a t -> Type where
  SATc         :: forall a (ta :: a). Sing ta -> Sing ('ATc ta)
  SATKey       :: forall a (ta :: a). Sing ta -> Sing ('ATKey ta)
  SATUnit      :: forall a (ta :: a). Sing ta -> Sing ('ATUnit ta)
  SATSignature :: forall a (ta :: a). Sing ta -> Sing ('ATSignature ta)
  SATChainId   :: forall a (ta :: a). Sing ta -> Sing ('ATChainId ta)
  SATOption    :: forall a t (ta :: a) (xs :: Annotated a t). Sing ta -> Sing xs -> Sing ('ATOption ta xs)
  SATList      :: forall a t (ta :: a) (xs :: Annotated a t). Sing ta -> Sing xs -> Sing ('ATList ta xs)
  SATSet       :: forall a (ta :: a) (tb :: a). Sing ta -> Sing tb -> Sing ('ATSet ta tb)
  SATOperation :: forall a (ta :: a). Sing ta -> Sing ('ATOperation ta)
  SATContract  :: forall a t (ta :: a) (xs :: Annotated a t). Sing ta -> Sing xs -> Sing ('ATContract ta xs)
  SATLambda    :: forall a s t (ta :: a) (xs :: Annotated a s) (ys :: Annotated a t). Sing ta -> Sing xs -> Sing ys -> Sing ('ATLambda ta xs ys)
  SATMap       :: forall a t (ta :: a) (tb :: a) (xs :: Annotated a t). Sing ta -> Sing tb -> Sing xs -> Sing ('ATMap ta tb xs)
  SATBigMap    :: forall a t (ta :: a) (tb :: a) (xs :: Annotated a t). Sing ta -> Sing tb -> Sing xs -> Sing ('ATBigMap ta tb xs)

instance (SingI ta) => SingI ('ATc ta) where
  sing = SATc sing
instance (SingI ta) => SingI ('ATKey ta) where
  sing = SATKey sing
instance (SingI ta) => SingI ('ATUnit ta) where
  sing = SATUnit sing
instance (SingI ta) => SingI ('ATSignature ta) where
  sing = SATSignature sing
instance (SingI ta) => SingI ('ATChainId ta) where
  sing = SATChainId sing
instance (SingI ta,  SingI xs) => SingI ('ATOption ta xs) where
  sing = SATOption sing sing
instance (SingI ta,  SingI xs) => SingI ('ATList ta xs) where
  sing = SATList sing sing
instance (SingI ta,  SingI tb) => SingI ('ATSet ta tb) where
  sing = SATSet sing sing
instance (SingI ta) => SingI ('ATOperation ta) where
  sing = SATOperation sing
instance (SingI ta,  SingI xs) => SingI ('ATContract ta xs) where
  sing = SATContract sing sing
instance (SingI ta,  SingI xs,  SingI ys) => SingI ('ATLambda ta xs ys) where
  sing = SATLambda sing sing sing
instance (SingI ta,  SingI tb,  SingI xs) => SingI ('ATMap ta tb xs) where
  sing = SATMap sing sing sing
instance (SingI ta,  SingI tb,  SingI xs) => SingI ('ATBigMap ta tb xs) where
  sing = SATBigMap sing sing sing

instance SingKind a => SingKind (AnnotatedOpq a t) where
  type Demote (AnnotatedOpq a t) = AnnotatedOpq (Demote a) t

  fromSing (SATc ta) = ATc (fromSing ta)
  fromSing (SATKey ta) = ATKey (fromSing ta)
  fromSing (SATUnit ta) = ATUnit (fromSing ta)
  fromSing (SATSignature ta) = ATSignature (fromSing ta)
  fromSing (SATChainId ta) = ATChainId (fromSing ta)
  fromSing (SATOption ta xs) = ATOption (fromSing ta) (fromSing xs)
  fromSing (SATList ta xs) = ATList (fromSing ta) (fromSing xs)
  fromSing (SATSet ta tb) = ATSet (fromSing ta) (fromSing tb)
  fromSing (SATOperation ta) = ATOperation (fromSing ta)
  fromSing (SATContract ta xs) = ATContract (fromSing ta) (fromSing xs)
  fromSing (SATLambda ta xs ys) = ATLambda (fromSing ta) (fromSing xs) (fromSing ys)
  fromSing (SATMap ta tb xs) = ATMap (fromSing ta) (fromSing tb) (fromSing xs)
  fromSing (SATBigMap ta tb xs) = ATBigMap (fromSing ta) (fromSing tb) (fromSing xs)

  toSing (ATc ta) =
    case toSing ta of
      SomeSing sta ->
        SomeSing $
        SATc sta
  toSing (ATKey ta) =
    case toSing ta of
      SomeSing sta ->
        SomeSing $
        SATKey sta
  toSing (ATUnit ta) =
    case toSing ta of
      SomeSing sta ->
        SomeSing $
        SATUnit sta
  toSing (ATSignature ta) =
    case toSing ta of
      SomeSing sta ->
        SomeSing $
        SATSignature sta
  toSing (ATChainId ta) =
    case toSing ta of
      SomeSing sta ->
        SomeSing $
        SATChainId sta
  toSing (ATOption ta xs) =
    case (toSing ta, toSing xs) of
      (SomeSing sta, SomeSing sxs) ->
        SomeSing $
        SATOption sta sxs
  toSing (ATList ta xs) =
    case (toSing ta, toSing xs) of
      (SomeSing sta, SomeSing sxs) ->
        SomeSing $
        SATList sta sxs
  toSing (ATSet ta tb) =
    case (toSing ta, toSing tb) of
      (SomeSing sta, SomeSing stb) ->
        SomeSing $
        SATSet sta stb
  toSing (ATOperation ta) =
    case toSing ta of
      SomeSing sta ->
        SomeSing $
        SATOperation sta
  toSing (ATContract ta xs) =
    case (toSing ta, toSing xs) of
      (SomeSing sta, SomeSing sxs) ->
        SomeSing $
        SATContract sta sxs
  toSing (ATLambda ta xs ys) =
    case (toSing ta, toSing xs, toSing ys) of
      (SomeSing sta, SomeSing sxs, SomeSing sys) ->
        SomeSing $
        SATLambda sta sxs sys
  toSing (ATMap ta tb xs) =
    case (toSing ta, toSing tb, toSing xs) of
      (SomeSing sta, SomeSing stb, SomeSing sxs) ->
        SomeSing $
        SATMap sta stb sxs
  toSing (ATBigMap ta tb xs) =
    case (toSing ta, toSing tb, toSing xs) of
      (SomeSing sta, SomeSing stb, SomeSing sxs) ->
        SomeSing $
        SATBigMap sta stb sxs

$(genPromotions [''AnnotatedOpq])

instance HasDict1 a => HasDict1 (AnnotatedOpq a t) where
  evidence1 = $(gen_evidence1 ''AnnotatedOpq)



-- | The following implementation of showsUnaryWith gets promoted automatically:
--
-- @
--  (\sp name d x -> showParen (d > 10) $ showString name . showString " " . sp 11 x)
-- @
--
-- See `Michelson.Typed.Annotation.Sing.Opq.TH` for singletons
instance Show a => Show (AnnotatedOpq a t) where
  showsPrec d (ATc ta) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATc" d (ta)
  showsPrec d (ATKey ta) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATKey" d (ta)
  showsPrec d (ATUnit ta) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATUnit" d (ta)
  showsPrec d (ATSignature ta) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATSignature" d (ta)
  showsPrec d (ATChainId ta) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATChainId" d (ta)
  showsPrec d (ATOption ta xs) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATOption" d (ta, xs)
  showsPrec d (ATList ta xs) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATList" d (ta, xs)
  showsPrec d (ATSet ta tb) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATSet" d (ta, tb)
  showsPrec d (ATOperation ta) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATOperation" d (ta)
  showsPrec d (ATContract ta xs) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATContract" d (ta, xs)
  showsPrec d (ATLambda ta xs ys) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATLambda" d (ta, xs, ys)
  showsPrec d (ATMap ta tb xs) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATMap" d (ta, tb, xs)
  showsPrec d (ATBigMap ta tb xs) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATBigMap" d (ta, tb, xs)

-- | `traverse` the annotation in `AnnotatedAlg`
--
-- See `Michelson.Typed.Annotation.Sing.Opq.TH` for singletons
traverseAnnotatedOpq :: Applicative f => (a -> f b) -> AnnotatedOpq a t -> f (AnnotatedOpq b t)
traverseAnnotatedOpq fs (ATc ta) = ATc <$> fs ta
traverseAnnotatedOpq fs (ATKey ta) = ATKey <$> fs ta
traverseAnnotatedOpq fs (ATUnit ta) = ATUnit <$> fs ta
traverseAnnotatedOpq fs (ATSignature ta) = ATSignature <$> fs ta
traverseAnnotatedOpq fs (ATChainId ta) = ATChainId <$> fs ta
traverseAnnotatedOpq fs (ATOption ta xs) = ATOption <$> fs ta <*> traverseAnnotated fs xs
traverseAnnotatedOpq fs (ATList ta xs) = ATList <$> fs ta <*> traverseAnnotated fs xs
traverseAnnotatedOpq fs (ATSet ta tb) = ATSet <$> fs ta <*> fs tb
traverseAnnotatedOpq fs (ATOperation ta) = ATOperation <$> fs ta
traverseAnnotatedOpq fs (ATContract ta xs) = ATContract <$> fs ta <*> traverseAnnotated fs xs
traverseAnnotatedOpq fs (ATLambda ta xs ys) = ATLambda <$> fs ta <*> traverseAnnotated fs xs <*> traverseAnnotated fs ys
traverseAnnotatedOpq fs (ATMap ta tb xs) = ATMap <$> fs ta <*> fs tb <*> traverseAnnotated fs xs
traverseAnnotatedOpq fs (ATBigMap ta tb xs) = ATBigMap <$> fs ta <*> fs tb <*> traverseAnnotated fs xs

