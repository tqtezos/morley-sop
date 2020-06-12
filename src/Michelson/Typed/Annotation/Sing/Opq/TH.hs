{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-type-patterns -Wno-orphans #-}

-- | See `Michelson.Typed.Annotation.Sing.Opq` for documentation
module Michelson.Typed.Annotation.Sing.Opq.TH where

import Data.String
import Prelude hiding (show)
import Text.Show

import Data.Singletons.Prelude
import Data.Singletons.TH

import Michelson.Typed.Annotation.Sing (traverseAnnotated)
import Michelson.Typed.Annotation.Sing.TH (TraverseAnnotatedSym0, sTraverseAnnotated)
import Michelson.Typed.Annotation.Sing.Opq


$(singletonsOnly [d|
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

  |])

