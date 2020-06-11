{-# OPTIONS -Wno-missing-export-lists #-}

module Test.Michelson.Typed.Annotation.Sing.Alg where
import Michelson.Typed.Annotation.Sing.Alg

import Michelson.Typed.Value.Arbitrary ()

import Data.Constraint.HasDict1

import Prelude
import Data.Singletons
import Data.Singletons.TypeLits

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (property)

import Test.Data.Singletons.SingKind

test_IsoSingKind :: TestTree
test_IsoSingKind = testProperty "Iso SingKind" $
  property $ \t ->
  case toSing t of
    SomeSing (st :: Sing t') ->
      withDict1 st $
      property $
      \(x :: AnnotatedAlg Text t') ->
      isoSingKind @(AnnotatedAlg Symbol t') x

