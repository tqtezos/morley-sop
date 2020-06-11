{-# OPTIONS -Wno-missing-export-lists #-}

module Test.Michelson.Typed.Annotation.Sing.Notes where
import Michelson.Typed.Annotation.Sing.Notes

import Michelson.Typed.Annotation.Sing
import Michelson.Typed.Value.Arbitrary ()

import Data.Constraint.HasDict1

import Prelude
import Data.Singletons

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (property)

import Test.Iso

test_IsoAnnotatedToNotes :: TestTree
test_IsoAnnotatedToNotes = testProperty "Iso annotatedToNotes" $
  property $ \t ->
  case toSing t of
    SomeSing (st :: Sing t') ->
      withDict1 st $
      property $
      \(x :: Annotated Text t') ->
      propIso annotatedToNotes annotatedFromNotes x

