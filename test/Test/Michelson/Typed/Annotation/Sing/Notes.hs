

module Test.Michelson.Typed.Annotation.Sing.Notes where
import Michelson.Typed.Annotation.Sing.Notes

import Michelson.Typed.Annotation.Sing
import Michelson.Typed.T.Sing
import Michelson.Typed.Value.Arbitrary

import Michelson.Typed.T
-- import Michelson.Test.Gen
import Util.Test.Arbitrary

import Data.Eq
import Data.String
import Data.Function
import Text.Show

import Data.Constraint.HasDict1

import Data.Singletons
import Data.Singletons.TypeLits
import Data.Text

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Property, testProperty)
import Test.QuickCheck (property)

import Test.Data.Singletons.SingKind
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

