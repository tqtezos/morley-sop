{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-type-patterns -Wno-orphans #-}

-- | See `Michelson.Typed.Annotation.Sing.Alg` for documentation
module Michelson.Typed.Annotation.Sing.Alg.TH where

import Data.String
import Prelude hiding (show)

import Text.Show

import Data.Singletons.Prelude
import Data.Singletons.TH

import Michelson.Typed.Annotation.Sing.Notes ()
import Michelson.Typed.Annotation.Sing.Opq
import Michelson.Typed.Annotation.Sing.Opq.TH
import Michelson.Typed.Annotation.Sing.Alg


$(singletonsOnly [d|
  instance Show a => Show (AnnotatedAlg a t) where
    showsPrec d (ATPair x1 x2 x3 x4 x5) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATPair" d (x1, x2, x3, x4, x5)
    showsPrec d (ATOr x1 x2 x3 x4 x5) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATOr" d (x1, x2, x3, x4, x5)
    showsPrec d (ATOpq x1) = (\sp name d' x -> showParen (d' > 10) $ showString name . showString " " . sp 11 x) showsPrec "ATOpq" d x1

  traverseAnnotatedAlg :: Applicative f => (a -> f b) -> AnnotatedAlg a t -> f (AnnotatedAlg b t)
  traverseAnnotatedAlg fs (ATPair x1 x2 x3 x4 x5) = ATPair <$> fs x1 <*> fs x2 <*> fs x3 <*> traverseAnnotatedAlg fs x4 <*> traverseAnnotatedAlg fs x5
  traverseAnnotatedAlg fs (ATOr x1 x2 x3 x4 x5) = ATOr <$> fs x1 <*> fs x2 <*> fs x3 <*> traverseAnnotatedAlg fs x4 <*> traverseAnnotatedAlg fs x5
  traverseAnnotatedAlg fs (ATOpq x1) = ATOpq <$> traverseAnnotatedOpq fs x1

  |])


