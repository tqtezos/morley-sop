{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Test.Michelson.Typed.Annotation.Path where
import Michelson.Typed.Annotation.Path

import Lorentz.Value
import Michelson.Typed.Scope

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Function
import Data.List
import Data.Semigroup
import Data.Tuple
import Data.String
import Data.Type.Equality
import Prelude (Eq(..), Ord(..), Num(..))
import Text.Show

import Data.AltError
import Data.Constraint.HasDict1
import Michelson.Typed.Annotation.Sing
import Michelson.Typed.Annotation.Sing.Alg
import Michelson.Typed.EntryPoints.Sing.Alg.Paths
import Michelson.Typed.T.Alg
import Michelson.Typed.T.Sing
import Michelson.Typed.Value.Arbitrary ()

import Control.Lens.Getter (view)
import Data.Constraint
import Data.Singletons
import Data.Singletons.Prelude
import Data.Text (Text)
import qualified Data.Text as T
-- import Text.Megaparsec

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Discard(..), testProperty)
import Test.Tasty.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Test.Iso

-- test_IsoEpPath :: TestTree
-- test_IsoEpPath = testProperty "Iso ppPath" $
--   property $ \path ->
--     propIsoWithMiddle
--     (("middle: \n"<>) . show)
--     (fmap ppPath)
--     (>>= (>>= fromPathExpr) . first show . parse expr "test_file" . T.unpack)
--     (pure path)

sortedAllUnique :: (Ord a, Show a) => [a] -> Property
sortedAllUnique [] = property ()
sortedAllUnique (x:xs) = loop x xs
  where
    loop _ [] = property ()
    loop y (z:zs) = y =/= z .&&. loop z zs

allUnique :: (Ord a, Show a) => [a] -> Property
allUnique = sortedAllUnique . sort

test_Injective_ppPath :: TestTree
test_Injective_ppPath = testGroup "ppPath should be injective"
  [ testProperty "direct implication" $
    mapSize (const 5) $
    withMaxSuccess 10 $ -- most inputs are discarded, even with size = 5
    property $ \(pathX :: Path Text) pathY ->
      (ppPath pathX == ppPath pathY) ==> (pathX == pathY)
  , testProperty "on sEpPaths" $
    property $ \t ->
    case toSing t of
      SomeSing (st :: Sing t') ->
        case opAbsense st of
          Nothing -> property Discard
          Just Dict ->
            case nestedBigMapsAbsense st of
              Nothing -> property Discard
              Just Dict ->
                case fromToTAlg st of
                  Refl ->
                    withDict1 st $
                    let singToTAlgSt = singToTAlg st in
                    withDict1 singToTAlgSt $
                    withDict (singTypeableT st) $
                    forbiddenOp @t' $
                    forbiddenNestedBigMaps @t' $
                    property $
                    \(ann :: Annotated Text t') ->
                    case toSing ann of
                      SomeSing (sann :: Sing ann') ->
                        allUnique . fmap ppPath . fromSing . sEpPaths $
                        (sUniqifyEpPathsSimpler (singToAnnotatedAlg sann) :: Sing ((UniqifyEpPathsSimpler (ToAnnotatedAlg ann'))))
  ]

test_Ord_Path :: TestTree
test_Ord_Path = testBatch $ ord (const $ arbitrary @(Path Text))

