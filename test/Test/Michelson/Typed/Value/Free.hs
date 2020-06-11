{-# OPTIONS -Wno-missing-export-lists #-}

module Test.Michelson.Typed.Value.Free where
import Michelson.Typed.Value.Free

import Michelson.Typed.T.Alg
import Michelson.Typed.T.Sing
import Michelson.Typed.Value.Arbitrary ()

import Lorentz.Value
import Michelson.Typed.Scope

import Data.Type.Equality

import Data.AltError
import Data.Constraint.HasDict1

import Data.Constraint
import Data.Singletons

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Discard(..), testProperty)
import Test.QuickCheck (property)

import Test.Iso

test_IsoValueAlg :: TestTree
test_IsoValueAlg = testProperty "Iso toValueAlg" $
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
                  withDict (singTypeableT st) $
                  forbiddenOp @t' $
                  forbiddenNestedBigMaps @t' $
                  property $
                  \(x :: Value t') ->
                  propIso toValueAlg fromValueAlg x

test_IsoValueAlgT :: TestTree
test_IsoValueAlgT = testProperty "Iso toValueAlgT" $
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
                  withDict1 (singToTAlg st) $
                  withDict (singTypeableT st) $
                  forbiddenOp @t' $
                  forbiddenNestedBigMaps @t' $
                  property $
                  \(x :: Value t') ->
                  propIso
                  (fmap (toValueAlgT (singToTAlg st) . toValueAlg))
                  (>>= fmap fromValueAlg . runValueAlgT)
                  (pure x :: AltE [String] (Value t'))

