-- {-# LANGUAGE NoTemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.EntryPoints.Sing.Alg.Types where

import Prelude hiding (unwords, show)

import Control.AltError
import Data.AltError

import Michelson.Typed.Annotation.Path
import Michelson.Typed.EntryPoints.Error

import Michelson.Typed.Annotation.Sing.Alg
import Michelson.Typed.T.Alg

import Data.Singletons.TH
import Data.Singletons.TypeLits


type SymAnn = AnnotatedAlg Symbol

$(singletonsOnly [d|

  -- Either we have the expected field and return the input or fail with `EpFieldTFieldError`
  epFieldTFieldEq :: TOpq -> Symbol -> Symbol -> Bool -> ErrM TOpq
  epFieldTFieldEq t _ _ True = pure t
  epFieldTFieldEq _ fieldNameA fieldNameB False = altErr [epFieldTFieldError fieldNameA fieldNameB]

  -- This fails with an error, because otherwise the @epPath@ is invalid
  epFieldTAssertHere :: TOpq -> EpPath -> ErrM TOpq -> ErrM TOpq
  epFieldTAssertHere _ _ (AltThrow xs) = AltThrow xs
  epFieldTAssertHere _ _ (AltExcept xs) = AltExcept xs
  epFieldTAssertHere _ Here (PureAltE xs) = PureAltE xs
  epFieldTAssertHere t ((:*) xs ys) (PureAltE _) = altFail [epFieldTAssertHereError t ((:*) xs ys)]
  epFieldTAssertHere t ((:+) xs ys) (PureAltE _) = altFail [epFieldTAssertHereError t ((:+) xs ys)]

  epFieldTEntrypointEq :: forall t. TAlg -> SymAnn t -> EpPath -> Symbol -> Symbol -> Symbol -> Bool -> ErrM TOpq
  epFieldTEntrypointEq t ann epPath fieldName _ _ True = epFieldT t ann epPath fieldName
  epFieldTEntrypointEq _ ann epPath fieldName entrypointNameA entrypointNameB False =
    altErr [epFieldTEntrypointError ann epPath fieldName entrypointNameA entrypointNameB]

  epFieldTResolveOr :: forall ta tb. (TAlg, TAlg) -> Symbol -> Symbol -> SymAnn ta -> SymAnn tb -> EpPath -> Symbol -> ErrM TOpq
  epFieldTResolveOr (ta, tb) aa ab as bs ((:+) entrypointName epPath) fieldName =
    epFieldTEntrypointEq ta as epPath fieldName aa entrypointName (aa == entrypointName) <||>
    epFieldTEntrypointEq tb bs epPath fieldName ab entrypointName (ab == entrypointName)
  epFieldTResolveOr _ aa ab as bs ((:*) xs ys) fieldName =
    altFail [epFieldTResolveOrError aa ab as bs ((:*) xs ys) fieldName]
  epFieldTResolveOr _ aa ab as bs Here fieldName =
    altFail [epFieldTResolveOrError aa ab as bs Here fieldName]

  epFieldTResolvePair :: forall ta tb. TAlg -> TAlg -> SymAnn ta -> SymAnn tb -> EpPath -> Symbol -> ErrM TOpq
  epFieldTResolvePair ta tb as bs ((:*) epPathA epPathB) fieldName =
    epFieldT ta as epPathA fieldName <||>
    epFieldT tb bs epPathB fieldName
  epFieldTResolvePair _ _ as bs ((:+) xs ys) fieldName =
    altFail [epFieldTResolvePairError as bs ((:+) xs ys) fieldName]
  epFieldTResolvePair _ _ as bs Here fieldName =
    altFail [epFieldTResolvePairError as bs Here fieldName]

  epFieldT :: forall t. TAlg -> SymAnn t -> EpPath -> Symbol -> ErrM TOpq
  epFieldT (TOr ta tb) (ATOr _ aa ab as bs) epPath fieldName = epFieldTResolveOr (ta, tb) aa ab as bs epPath fieldName
  epFieldT (TPair ta tb) (ATPair _ _ _ as bs) epPath fieldName = epFieldTResolvePair ta tb as bs epPath fieldName
  epFieldT (TOpq t1) (ATOpq ta) epPath tb = epFieldTAssertHere t1 epPath (epFieldTFieldEq t1 (tOpqTypeAnn ta) tb (tOpqTypeAnn ta == tb))

  |])

