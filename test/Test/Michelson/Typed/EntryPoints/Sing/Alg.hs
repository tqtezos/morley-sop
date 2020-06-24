{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Test.Michelson.Typed.EntryPoints.Sing.Alg where
import Michelson.Typed.EntryPoints.Sing.Alg

import Michelson.Typed.Annotation.Sing
import Michelson.Typed.Annotation.Sing.Alg
import Michelson.Typed.EntryPoints.Sing.Alg.Types.TH
import Michelson.Typed.T.Alg
import Michelson.Typed.T.Sing
import Michelson.Typed.Value.Arbitrary ()
import Michelson.Typed.Value.Free

import Lorentz.Value
import Michelson.Typed.Scope

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Semigroup
import Data.String
import Data.Type.Equality
import Text.Show

import Data.AltError
import Data.Constraint.HasDict1

import Control.Lens.Getter (view)
import Data.Constraint
import Data.Singletons
import Data.Singletons.Prelude
import Data.Text (Text)

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Discard(..), testProperty)
import Test.QuickCheck

import Test.Iso

test_IsoEpValue :: TestTree
test_IsoEpValue = testProperty "Iso runEpValue" $
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
                  \(ann :: Annotated Text t') (x :: Value t') ->
                  case toSing ann of
                    SomeSing (sann :: Sing ann') ->
                      let singToAnnotatedAlgSann = ((sUniqifyEpPaths (singToAnnotatedAlg sann)) :: Sing ((UniqifyEpPaths (ToAnnotatedAlg ann')))) in
                      counterexample (("epPaths: "<>) . show $ fromSing (sEpPaths (singToAnnotatedAlg sann))) $
                      counterexample (("uniqified epPaths: "<>) . show $ fromSing (sEpPaths singToAnnotatedAlgSann)) $
                      counterexample (("uniqified epPaths fields: \n"<>) . unlines . fmap show $
                        fromSing (singFun1 @(EpFieldNamesErrMSym2 (ToTAlg t') ((UniqifyEpPaths (ToAnnotatedAlg ann')))) (sEpFieldNamesErrM singToTAlgSt singToAnnotatedAlgSann) `sFmap` sEpPaths singToAnnotatedAlgSann)) $
                      counterexample (("uniqified annotation: "<>) . show $ fromSing ((sUniqifyEpPaths (singToAnnotatedAlg sann)))) $

                      withDict @_ @_ @Property (prfAllShowEpFields singToTAlgSt singToAnnotatedAlgSann (sEpPaths singToAnnotatedAlgSann)) $
                      propIsoWithMiddle
                      (("middle: \n"<>) . show)
                      (>>= (fromEpValueF singToTAlgSt singToAnnotatedAlgSann .
                        view (lensEpValueF singToTAlgSt singToAnnotatedAlgSann)) .
                        toValueAlgT singToTAlgSt . toValueAlg
                      )
                      (>>= runEpValue singToTAlgSt singToAnnotatedAlgSann)
                      (pure x :: AltE [String] (Value t'))

