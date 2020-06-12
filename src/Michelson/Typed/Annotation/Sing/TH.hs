{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}

{-# OPTIONS -Wno-unused-type-patterns #-} -- `singletons` for (Show (Annotated a t)) generates unused type patterns

-- | See `Michelson.Typed.Annotation.Sing` for documentation
module Michelson.Typed.Annotation.Sing.TH where

import Prelude
import Text.Show

import Data.Singletons.Prelude
import Data.Singletons.TH

import Michelson.Typed.Annotation.Sing


$(singletonsOnly [d|
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

