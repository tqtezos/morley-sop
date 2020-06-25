{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Test.Michelson.Typed.EntryPoints.Sing.Alg.Types where
import Michelson.Typed.EntryPoints.Sing.Alg.Types

import Michelson.Typed.Annotation.Sing
import Michelson.Typed.Annotation.Sing.Alg
import Michelson.Typed.EntryPoints.Sing.Alg.Types.TH
import Michelson.Typed.T.Alg
import Michelson.Typed.T.Sing
import Michelson.Typed.Value.Arbitrary ()

import Lorentz.Value
import Michelson.Typed.Scope

import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import Data.Semigroup
import Data.Tuple
import Data.String
import Data.Type.Equality
import Text.Show

import Data.Constraint.HasDict1

import Data.Constraint
import Data.Singletons
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State.Strict

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Discard(..), testProperty)
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- | Assert that `epPathsAbbrev` and `epPaths` produce the same non-abbreviated paths
expectedAbbreviation :: forall s t. (Ord s, Show s) => AnnotatedAlg s t -> Property
expectedAbbreviation = liftM2 ((===) . snd) (unzip . epPathsAbbrev) epPaths

-- | Assert that all elements are unique
allUnique :: forall a. (Ord a, Show a) => [a] -> Property
allUnique =
  monadicIO . -- IO not required, but GHC can't infer the type for ST
  flip evalStateT Map.empty .
  mapM_
    (\elem' -> do
       get >>=
         Map.alterF
           (maybe
              (return (Just elem'))
              (fail $
               unwords ["allUnique: found non-unique element:", show elem']))
           elem' >>=
         put)

-- | Assert that all abbreviations produced by `epPathsAbbrev` are unique
uniqueAbbreviations :: forall s t. (Ord s, Show s) => AnnotatedAlg s t -> Property
uniqueAbbreviations = allUnique . fmap fst . epPathsAbbrev

test_ExpectedAbbreviation :: TestTree
test_ExpectedAbbreviation = testProperty "epPathsAbbrev strictly extends epPaths" $
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
                      let singToAnnotatedAlgSann = ((sUniqifyEpPaths (singToAnnotatedAlg sann)) :: Sing ((UniqifyEpPaths (ToAnnotatedAlg ann')))) in
                      counterexample (("epPaths: "<>) . show $ fromSing (sEpPaths (singToAnnotatedAlg sann))) $
                      counterexample (("uniqified epPaths: "<>) . show $ fromSing (sEpPaths singToAnnotatedAlgSann)) $
                      expectedAbbreviation $ fromSing singToAnnotatedAlgSann

test_UniqueAbbreviations :: TestTree
test_UniqueAbbreviations = testProperty "epPathsAbbrev's abbreivations are all unique" $
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
                      let singToAnnotatedAlgSann = ((sUniqifyEpPaths (singToAnnotatedAlg sann)) :: Sing ((UniqifyEpPaths (ToAnnotatedAlg ann')))) in
                      counterexample (("epPaths: "<>) . show $ fromSing (sEpPaths (singToAnnotatedAlg sann))) $
                      counterexample (("uniqified epPaths: "<>) . show $ fromSing (sEpPaths singToAnnotatedAlgSann)) $
                      uniqueAbbreviations $ fromSing singToAnnotatedAlgSann

