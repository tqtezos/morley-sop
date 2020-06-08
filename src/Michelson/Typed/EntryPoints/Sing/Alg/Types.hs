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
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Maybe
import Data.Singletons.Prelude.Monad
import Data.Singletons.Prelude.Bool

type SymAnn = AnnotatedAlg Symbol

$(singletonsOnly [d|

  epFieldRecResolveOr :: forall f r ta tb. (AltError [Symbol] f, Show r) => (Maybe Symbol, TOpq -> Maybe Symbol -> f r) -> (TAlg, TAlg) -> (Symbol, Symbol) -> SymAnn ta -> SymAnn tb -> EpPath -> Maybe Symbol -> f r
  epFieldRecResolveOr (name, fs) (ta, tb) (aa, ab) as bs ((:+) entrypointName epPath) fieldAnn =
    (bool_
      (altErr [epFieldRecEntrypointError as epPath name aa entrypointName fieldAnn])
      (epFieldRec' name fs ta as epPath (Just aa))
      (aa == entrypointName)
    ) <||>
    (bool_
      (altErr [epFieldRecEntrypointError bs epPath name ab entrypointName fieldAnn])
      (epFieldRec' name fs tb bs epPath (Just ab))
      (ab == entrypointName)
    )
  epFieldRecResolveOr (name, _fs) _tab (aa, ab) as bs ((:*) xs ys) fieldAnn =
    altFail [epFieldRecResolveOrError aa ab as bs ((:*) xs ys) name fieldAnn]
  epFieldRecResolveOr (name, _fs) _tab (aa, ab) as bs Here fieldAnn =
    altFail [epFieldRecResolveOrError aa ab as bs Here name fieldAnn]

  epFieldRecResolvePair :: forall f r ta tb. (AltError [Symbol] f, Show r) => (Maybe Symbol, TOpq -> Maybe Symbol -> f r) -> (TAlg, TAlg) -> (Symbol, Symbol) -> SymAnn ta -> SymAnn tb -> EpPath -> Maybe Symbol -> f r
  epFieldRecResolvePair (name, fs) (ta, tb) (aa, ab) as bs ((:*) epPathA epPathB) _fieldAnn =
    epFieldRec' name fs ta as epPathA (Just aa) <||>
    epFieldRec' name fs tb bs epPathB (Just ab)
  epFieldRecResolvePair (name, _fs) (_ta, _tb) (_aa, _ab) as bs ((:+) xs ys) fieldAnn =
    altFail [epFieldRecResolvePairError as bs ((:+) xs ys) name fieldAnn]
  epFieldRecResolvePair (name, _fs) (_ta, _tb) (_aa, _ab) as bs Here fieldAnn =
    altFail [epFieldRecResolvePairError as bs Here name fieldAnn]

  epFieldRec' :: forall f r t. (AltError [Symbol] f, Show r) => Maybe Symbol -> (TOpq -> Maybe Symbol -> f r) -> TAlg -> SymAnn t -> EpPath -> Maybe Symbol -> f r
  epFieldRec' name fs (TOr ta tb) (ATOr _ aa ab as bs) epPath fieldAnn = epFieldRecResolveOr (name, fs) (ta, tb) (aa, ab) as bs epPath fieldAnn
  epFieldRec' name fs (TPair ta tb) (ATPair _ aa ab as bs) epPath fieldAnn = epFieldRecResolvePair (name, fs) (ta, tb) (aa, ab) as bs epPath fieldAnn
  epFieldRec' _name fs (TOpq t1) (ATOpq _ta) epPath fieldAnn =
    bool_ (altFail [epFieldRecAssertHereError t1 epPath fieldAnn]) (pure ()) (epPath == Here) *>
    (fs t1 fieldAnn)

  epFieldRec :: forall f r t. (AltError [Symbol] f, Show r) => Maybe Symbol -> (TOpq -> Maybe Symbol -> f r) -> TAlg -> SymAnn t -> EpPath -> f r
  epFieldRec name fs t ann epPath = epFieldRec' name fs t ann epPath Nothing

  -- helper for epFieldT
  epFieldRecT :: forall f. AltError [Symbol] f => Maybe Symbol -> TOpq -> Maybe Symbol -> f TOpq
  epFieldRecT fieldName ta fieldNameA =
    bool_
      (altErr [epFieldTFieldError fieldName fieldNameA])
      (pure ta)
      (fieldName == fieldNameA)

  epFieldTResolveOr :: forall f ta tb. AltError [Symbol] f => (TAlg, TAlg) -> (Symbol, Symbol) -> SymAnn ta -> SymAnn tb -> EpPath -> Maybe Symbol -> Maybe Symbol -> f TOpq
  epFieldTResolveOr (ta, tb) (aa, ab) as bs epPath fieldName = epFieldRecResolveOr (fieldName, epFieldRecT fieldName) (ta, tb) (aa, ab) as bs epPath

  epFieldTResolvePair :: forall f ta tb. AltError [Symbol] f => (TAlg, TAlg) -> (Symbol, Symbol) -> SymAnn ta -> SymAnn tb -> EpPath -> Maybe Symbol -> Maybe Symbol -> f TOpq
  epFieldTResolvePair (ta, tb) (aa, ab) as bs epPath fieldName = epFieldRecResolvePair (fieldName, epFieldRecT fieldName) (ta, tb) (aa, ab) as bs epPath

  -- The type of an entrypoint field
  epFieldT' :: forall f t. AltError [Symbol] f => TAlg -> SymAnn t -> EpPath -> Maybe Symbol -> Maybe Symbol -> f TOpq
  epFieldT' t ann epPath fieldName = epFieldRec' fieldName (epFieldRecT fieldName) t ann epPath

  -- The type of an entrypoint field
  epFieldT :: forall f t. AltError [Symbol] f => TAlg -> SymAnn t -> EpPath -> Maybe Symbol -> f TOpq
  epFieldT t ann epPath fieldName = epFieldRec fieldName (epFieldRecT fieldName) t ann epPath


  -- helper for epFields
  epFieldRecFields :: forall f. AltError [Symbol] f => TOpq -> Maybe Symbol -> f (Maybe Symbol)
  epFieldRecFields _t = pure

  -- All entrypoint field names
  epFieldNames :: forall f t. AltError [Symbol] f => TAlg -> SymAnn t -> EpPath -> f (Maybe Symbol)
  epFieldNames = epFieldRec (Just "epFieldNames") epFieldRecFields


  -- All entrypoint field names, in ErrM
  epFieldNamesErrM :: forall t. TAlg -> SymAnn t -> EpPath -> ErrM [Maybe Symbol]
  epFieldNamesErrM t ann epPath = sort <$> listEToErrM (epFieldNames t ann epPath)

  epFieldTs :: forall t. TAlg -> SymAnn t -> EpPath -> ErrM [TOpq]
  epFieldTs t ann epPath =
    epFieldNamesErrM t ann epPath >>=
    traverse (epFieldT t ann epPath)

  |])


