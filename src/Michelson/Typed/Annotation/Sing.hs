{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS -Wno-missing-export-lists #-}

{-# OPTIONS -fmax-pmcheck-iterations=20000000 #-} -- type-checking this module takes extra-long
{-# OPTIONS -Wno-unused-type-patterns #-} -- `singletons` for (Show (Annotated a t)) generates unused type patterns

module Michelson.Typed.Annotation.Sing where

import Prelude
import Data.Kind
import Text.Show

import Michelson.Typed.T

import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TH

import Data.Constraint.HasDict1
import Data.Singletons.Prelude.Monad.State


-- | A generalization of `Notes` that's more amenable to `Sing`, `SingKind`
data Annotated a t where
  ATc         :: a -> Annotated a ('Tc ct)
  ATKey       :: a -> Annotated a 'TKey
  ATUnit      :: a -> Annotated a 'TUnit
  ATSignature :: a -> Annotated a 'TSignature
  ATChainId   :: a -> Annotated a 'TChainId
  ATOption    :: a -> Annotated a t -> Annotated a ('TOption t)
  ATList      :: a -> Annotated a t -> Annotated a ('TList t)
  ATSet       :: a -> a -> Annotated a ('TSet ct)
  ATOperation :: a -> Annotated a 'TOperation
  ATContract  :: a -> Annotated a t -> Annotated a ('TContract t)
  ATPair      :: a -> a -> a
              -> Annotated a p -> Annotated a q -> Annotated a ('TPair p q)
  ATOr        :: a -> a -> a
              -> Annotated a p -> Annotated a q -> Annotated a ('TOr p q)
  ATLambda    :: a -> Annotated a p -> Annotated a q -> Annotated a ('TLambda p q)
  ATMap       :: a -> a -> Annotated a v -> Annotated a ('TMap k v)
  ATBigMap    :: a -> a -> Annotated a v -> Annotated a ('TBigMap k v)

$(genPromotions [''Annotated])

$(singletons [d|
  instance Show a => Show (Annotated a t) where
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
    showsPrec d (ATPair x1 x2 x3 x4 x5) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATPair" d (x1, x2, x3, x4, x5)
    showsPrec d (ATOr x1 x2 x3 x4 x5) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATOr" d (x1, x2, x3, x4, x5)

  traverseAnnotated :: Applicative f => (a -> f b) -> Annotated a t -> f (Annotated b t)
  traverseAnnotated fs (ATc ta) = ATc <$> fs ta
  traverseAnnotated fs (ATKey ta) = ATKey <$> fs ta
  traverseAnnotated fs (ATUnit ta) = ATUnit <$> fs ta
  traverseAnnotated fs (ATSignature ta) = ATSignature <$> fs ta
  traverseAnnotated fs (ATChainId ta) = ATChainId <$> fs ta
  traverseAnnotated fs (ATOption ta xs) = ATOption <$> fs ta <*> traverseAnnotated fs xs
  traverseAnnotated fs (ATList ta xs) = ATList <$> fs ta <*> traverseAnnotated fs xs
  traverseAnnotated fs (ATSet ta tb) = ATSet <$> fs ta <*> fs tb
  traverseAnnotated fs (ATOperation ta) = ATOperation <$> fs ta
  traverseAnnotated fs (ATContract ta xs) = ATContract <$> fs ta <*> traverseAnnotated fs xs
  traverseAnnotated fs (ATLambda ta xs ys) = ATLambda <$> fs ta <*> traverseAnnotated fs xs <*> traverseAnnotated fs ys
  traverseAnnotated fs (ATMap ta tb xs) = ATMap <$> fs ta <*> fs tb <*> traverseAnnotated fs xs
  traverseAnnotated fs (ATBigMap ta tb xs) = ATBigMap <$> fs ta <*> fs tb <*> traverseAnnotated fs xs
  traverseAnnotated fs (ATPair x1 x2 x3 x4 x5) = ATPair <$> fs x1 <*> fs x2 <*> fs x3 <*> traverseAnnotated fs x4 <*> traverseAnnotated fs x5
  traverseAnnotated fs (ATOr x1 x2 x3 x4 x5) = ATOr <$> fs x1 <*> fs x2 <*> fs x3 <*> traverseAnnotated fs x4 <*> traverseAnnotated fs x5

    |])

data instance Sing :: Annotated a t -> Type where
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
  SATPair      :: forall a s t (ta :: a) (tb :: a) (tc :: a) (xs :: Annotated a s) (ys :: Annotated a t). Sing ta -> Sing tb -> Sing tc -> Sing xs -> Sing ys -> Sing ('ATPair ta tb tc xs ys)
  SATOr        :: forall a s t (ta :: a) (tb :: a) (tc :: a) (xs :: Annotated a s) (ys :: Annotated a t). Sing ta -> Sing tb -> Sing tc -> Sing xs -> Sing ys -> Sing ('ATOr ta tb tc xs ys)
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
instance (SingI ta,  SingI tb,  SingI tc,  SingI xs,  SingI ys) => SingI ('ATPair ta tb tc xs ys) where
  sing = SATPair sing sing sing sing sing
instance (SingI ta,  SingI tb,  SingI tc,  SingI xs,  SingI ys) => SingI ('ATOr ta tb tc xs ys) where
  sing = SATOr sing sing sing sing sing
instance (SingI ta,  SingI xs,  SingI ys) => SingI ('ATLambda ta xs ys) where
  sing = SATLambda sing sing sing
instance (SingI ta,  SingI tb,  SingI xs) => SingI ('ATMap ta tb xs) where
  sing = SATMap sing sing sing
instance (SingI ta,  SingI tb,  SingI xs) => SingI ('ATBigMap ta tb xs) where
  sing = SATBigMap sing sing sing

instance HasDict1 a => HasDict1 (Annotated a t) where
  evidence1 = $(gen_evidence1 ''Annotated)

instance SingKind a => SingKind (Annotated a t) where
  type Demote (Annotated a t) = Annotated (Demote a) t

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
  fromSing (SATPair ta tb tc xs ys) = ATPair (fromSing ta) (fromSing tb) (fromSing tc) (fromSing xs) (fromSing ys)
  fromSing (SATOr ta tb tc xs ys) = ATOr (fromSing ta) (fromSing tb) (fromSing tc) (fromSing xs) (fromSing ys)
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

