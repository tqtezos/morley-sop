-- {-# LANGUAGE NoTemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.EntryPoints.Sing.Alg.Types where

import Prelude hiding (unwords, show)

import Control.AltError
import Data.AltError
import Data.ListError

import Michelson.Typed.Annotation.Path
import Michelson.Typed.EntryPoints.Error

import Michelson.Typed.Annotation.Sing.Alg
import Michelson.Typed.T.Alg

import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Applicative
import Data.Singletons.Prelude.Monad
import Data.Singletons.Prelude.Semigroup
import Data.Singletons.Prelude.Bool

-- tt = _

type SymAnn = AnnotatedAlg Symbol

$(singletonsOnly [d|

  epFieldRecResolveOr :: forall f r ta tb. (AltError [Symbol] f, Show r) => (Symbol, TOpq -> Symbol -> f r) -> (TAlg, TAlg) -> (Symbol, Symbol) -> SymAnn ta -> SymAnn tb -> EpPath -> f r
  epFieldRecResolveOr (name, fs) (ta, tb) (aa, ab) as bs ((:+) entrypointName epPath) =
    bool_
      (altErr [epFieldRecEntrypointError as epPath name aa entrypointName])
      (epFieldRec name fs ta as epPath)
      (aa == entrypointName) <||>
    bool_
      (altErr [epFieldRecEntrypointError bs epPath name ab entrypointName])
      (epFieldRec name fs tb bs epPath)
      (ab == entrypointName)
  epFieldRecResolveOr (name, _fs) _tab (aa, ab) as bs ((:*) xs ys) =
    altFail [epFieldRecResolveOrError aa ab as bs ((:*) xs ys) name]
  epFieldRecResolveOr (name, _fs) _tab (aa, ab) as bs Here =
    altFail [epFieldRecResolveOrError aa ab as bs Here name]

  epFieldRecResolvePair :: forall f r ta tb. (AltError [Symbol] f, Show r) => Symbol -> (TOpq -> Symbol -> f r) -> TAlg -> TAlg -> SymAnn ta -> SymAnn tb -> EpPath -> f r
  epFieldRecResolvePair name fs ta tb as bs ((:*) epPathA epPathB) =
    epFieldRec name fs ta as epPathA <||>
    epFieldRec name fs tb bs epPathB
  epFieldRecResolvePair name _fs _ta _tb as bs ((:+) xs ys) =
    altFail [epFieldRecResolvePairError as bs ((:+) xs ys) name]
  epFieldRecResolvePair name _fs _ta _tb as bs Here =
    altFail [epFieldRecResolvePairError as bs Here name]

  epFieldRec :: forall f r t. (AltError [Symbol] f, Show r) => Symbol -> (TOpq -> Symbol -> f r) -> TAlg -> SymAnn t -> EpPath -> f r
  epFieldRec name fs (TOr ta tb) (ATOr _ aa ab as bs) epPath = epFieldRecResolveOr (name, fs) (ta, tb) (aa, ab) as bs epPath
  epFieldRec name fs (TPair ta tb) (ATPair _ _ _ as bs) epPath = epFieldRecResolvePair name fs ta tb as bs epPath
  epFieldRec _name fs (TOpq t1) (ATOpq ta) epPath =
    bool_ (altFail [epFieldRecAssertHereError t1 epPath]) (pure ()) (epPath == Here) *>
    (fs t1 (tOpqTypeAnn ta))


  -- helper for epFieldT
  epFieldRecT :: forall f. AltError [Symbol] f => Symbol -> TOpq -> Symbol -> f TOpq
  epFieldRecT fieldName ta fieldNameA =
    bool_
      (altErr [epFieldTFieldError fieldName fieldNameA])
      (pure ta)
      (fieldName == fieldNameA)

  epFieldTResolveOr :: forall f ta tb. AltError [Symbol] f => (TAlg, TAlg) -> (Symbol, Symbol) -> SymAnn ta -> SymAnn tb -> EpPath -> Symbol -> f TOpq
  epFieldTResolveOr (ta, tb) (aa, ab) as bs epPath fieldName = epFieldRecResolveOr (fieldName, epFieldRecT fieldName) (ta, tb) (aa, ab) as bs epPath

  epFieldTResolvePair :: forall f ta tb. AltError [Symbol] f => TAlg -> TAlg -> SymAnn ta -> SymAnn tb -> EpPath -> Symbol -> f TOpq
  epFieldTResolvePair ta tb as bs epPath fieldName = epFieldRecResolvePair fieldName (epFieldRecT fieldName) ta tb as bs epPath

  -- The type of an entrypoint field
  epFieldT :: forall f t. AltError [Symbol] f => TAlg -> SymAnn t -> EpPath -> Symbol -> f TOpq
  epFieldT t ann epPath fieldName = epFieldRec fieldName (epFieldRecT fieldName) t ann epPath


  -- helper for epFields
  epFieldRecFields :: forall f. AltError [Symbol] f => TOpq -> Symbol -> f Symbol
  epFieldRecFields _t = pure

  -- All entrypoint field names
  epFieldNames :: forall f t. AltError [Symbol] f => TAlg -> SymAnn t -> EpPath -> f Symbol
  epFieldNames = epFieldRec "epFieldNames" epFieldRecFields

  -- All entrypoint field names, in ErrM
  epFieldNamesErrM :: forall t. TAlg -> SymAnn t -> EpPath -> ErrM [Symbol]
  epFieldNamesErrM t ann epPath = listEToErrM (epFieldNames t ann epPath)

  epFieldTs :: forall t. TAlg -> SymAnn t -> EpPath -> ErrM [TOpq]
  epFieldTs t ann epPath =
    epFieldNamesErrM t ann epPath >>=
    traverse (epFieldT t ann epPath)

  |])

