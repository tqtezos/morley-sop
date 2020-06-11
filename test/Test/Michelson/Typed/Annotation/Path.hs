{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Test.Michelson.Typed.Annotation.Path where
import Michelson.Typed.Annotation.Path

import Lorentz.Value
import Michelson.Typed.Scope

import Control.Monad
import Data.Function
import Data.List
import Data.Type.Equality
import Prelude (Ord(..))
import Text.Show

import Data.Constraint.HasDict1
import Michelson.Typed.Annotation.Sing
import Michelson.Typed.Annotation.Sing.Alg
import Michelson.Typed.EntryPoints.Sing.Alg.Paths
import Michelson.Typed.T.Alg
import Michelson.Typed.T.Sing
import Michelson.Typed.Value.Arbitrary ()

import Data.Constraint
import Data.Singletons
import Data.Text (Text)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Discard(..), testProperty)
import Test.Tasty.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes


-- | Test that all elements of the list are unique, assuming the list is sorted
sortedAllUnique :: (Ord a, Show a) => [a] -> Property
sortedAllUnique [] = property ()
sortedAllUnique (x:xs) = loop x xs
  where
    loop _ [] = property ()
    loop y (z:zs) = y =/= z .&&. loop z zs

-- | Test that all elements of the list are unique
allUnique :: (Ord a, Show a) => [a] -> Property
allUnique = sortedAllUnique . sort

test_Injective_ppPath :: TestTree
test_Injective_ppPath = testGroup "ppPath should be injective"
  [ testProperty "on sEpPaths" $
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

