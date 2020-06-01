{-# LANGUAGE NoImplicitPrelude #-}

module Test.Michelson.Typed.EntryPoints.Sing.Alg where
import Michelson.Typed.EntryPoints.Sing.Alg

import Michelson.Typed.EntryPoints.Sing.Alg.Paths
import Michelson.Typed.T.Sing
import Michelson.Typed.T.Alg
import Michelson.Typed.Annotation.Sing
import Michelson.Typed.Annotation.Sing.Alg
import Michelson.Typed.EntryPoints.Sing.Alg.Fields
import Michelson.Typed.EntryPoints.Sing.Alg.Types
import Michelson.Typed.Value.Arbitrary ()
import Michelson.Typed.Value.Free

import qualified Michelson.Typed.Annotation.Sing as An
-- import qualified Michelson.Typed.T.Alg as Alg
import qualified Michelson.Typed.T as Michelson

import Lorentz.Value
import Michelson.Typed.Scope

import Data.Type.Equality
import Data.Semigroup
import Data.String
import Data.Function
import Data.Bifunctor
import Control.Applicative
import Control.Monad
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
import Test.QuickCheck -- (counterexample, property)

import Test.Iso

type ExampleT = 'Michelson.TOr 'Michelson.TUnit 'Michelson.TUnit

type ExampleAnn = 'An.ATOr "" "" "2" ('An.ATUnit "\r") ('An.ATUnit "\v")

tt =
  tupleToList $
  (fmap show . fromSing $ singFun1 @(EpFieldNamesErrMSym2 (ToTAlg ExampleT) (UniqifyEpPathsSimple ((ToAnnotatedAlg ExampleAnn))))
    (sEpFieldNamesErrM sst ssa) `sFmap`
  sEpPaths ssa
  , fmap show . fromSing $ sEpPaths ssa
  )
  where
    sst = singToTAlg (sing @ExampleT)
    ssa = sUniqifyEpPathsSimple ((singToAnnotatedAlg (sing @ExampleAnn)))
    tupleToList (x, y) = [x, y]

-- UniqifyEpPathsSimple
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
                      let singToAnnotatedAlgSann = ((sUniqifyEpPathsSimple (singToAnnotatedAlg sann)) :: Sing ((UniqifyEpPathsSimple (ToAnnotatedAlg ann')))) in -- singToAnnotatedAlg sann in
                      counterexample (("epPaths: "<>) . show $ fromSing (sEpPaths (singToAnnotatedAlg sann))) $
                      counterexample (("uniqified epPaths: "<>) . show $ fromSing (sEpPaths singToAnnotatedAlgSann)) $
                      counterexample (("uniqified epPaths fields: \n"<>) . unlines . fmap show $ fromSing (singFun1 @(EpFieldNamesErrMSym2 (ToTAlg t') ((UniqifyEpPathsSimple (ToAnnotatedAlg ann')))) (sEpFieldNamesErrM singToTAlgSt singToAnnotatedAlgSann) `sFmap` sEpPaths singToAnnotatedAlgSann)) $
                      -- counterexample (("uniqified epPaths: "<>) . show $ fromSing (sEpPaths (sUniqifyEpPathsSimple (sFieldToTypeAnn (singToAnnotatedAlg sann))))) $
                      -- counterexample (("fieldToTypeAnn: "<>) . show $ fromSing (sFieldToTypeAnn (singToAnnotatedAlg sann))) $
                      -- counterexample (("uniqified: "<>) . show $ fromSing singToAnnotatedAlgSann) $
                      counterexample (("uniqified2: "<>) . show $ fromSing ((sUniqifyEpPathsSimple (singToAnnotatedAlg sann)))) $
                      -- counterexample (("fields: "<>) . show $ fromSing (EpFieldNamesErrM singToTAlgSt singToAnnotatedAlgSann)) $
                      -- withDict (prfAllShowEpField @(AltE [String]) st singToAnnotatedAlgSann :: forall f t (ann :: SymAnn t) epPath xs. (forall t'. SingI t' => Show (f (ValueOpq t')))
  -- => Sing t -> Sing ann -> Sing epPath -> Sing xs -> Dict (SOP.All (SOP.Compose Show (EpField f t ann epPath)) xs)

                      -- :: forall t (ann :: SymAnn t) xs. Sing t -> Sing ann -> Sing xs -> Dict (SOP.All (SOP.Compose Show (EpFields I t ann)) xs)

                      withDict @_ @_ @Property (prfAllShowEpFields singToTAlgSt singToAnnotatedAlgSann (sEpPaths singToAnnotatedAlgSann)) $

                      propIsoWithMiddle
                      (("middle: \n"<>) . show)
                      (>>= (fromEpValueF singToTAlgSt singToAnnotatedAlgSann .
                        view (lensEpValueF singToTAlgSt singToAnnotatedAlgSann)) .
                        toValueAlgT singToTAlgSt . toValueAlg
                      )
                      (>>= runEpValue singToTAlgSt singToAnnotatedAlgSann)
                      (pure x :: AltE [String] (Value t'))


-- AltExcept
--   [ "(<||>) (PureAltE _) (PureAltE _):"
--   , "Z (EpFields _2 (WrapSing {unwrapSing = SCons (SSym @\"EpFieldRecAssertHereError \\nTUnit\\n \\n_2 :+ Here\\n\") SNil}))"
--   , "S (Z (EpFields  (WrapSing {unwrapSing = SCons (SSym @\"EpFieldRecResolveOr _ _ \\n\\n \\n_2\\n \\nATOpq (ATUnit \\r)\\n \\nATOpq (ATUnit \\v)\\n \\nHere\\n \\nepFieldNames\\n\") SNil})))"
--   ]

-- AltExcept
--   [ "(<||>) (PureAltE _) (PureAltE _):"
--   , "Z (EpFields _2 (WrapSing {unwrapSing = SCons (SSym @\"EpFieldRecAssertHereError \\nTUnit\\n \\n_2 :+ Here\\n\") SNil}))"
--   , "S (Z (EpFields  (WrapSing {unwrapSing = SCons (SSym @\"EpFieldRecResolveOr _ _ \\n\\n \\n_2\\n \\nATOpq (ATUnit \\r)\\n \\nATOpq (ATUnit \\v)\\n \\nHere\\n \\nepFieldNames\\n\") SNil})))"
--   ] /=
--   PureAltE (VOr (Right VUnit))
