
module Test.Michelson.Typed.Annotation.Sing.Alg where
import Michelson.Typed.Annotation.Sing.Alg

import Michelson.Typed.T.Sing
import Michelson.Typed.Value.Arbitrary

import Michelson.Typed.T
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

test_IsoSingKind :: TestTree
test_IsoSingKind = testProperty "Iso SingKind" $
  property $ \t ->
  case toSing t of
    SomeSing (st :: Sing t') ->
      withDict1 st $
      property $
      \(x :: AnnotatedAlg Text t') ->
      isoSingKind @(AnnotatedAlg Symbol t') x

