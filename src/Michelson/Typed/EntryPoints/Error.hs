{-# OPTIONS -Wno-missing-export-lists #-}

{-# LANGUAGE TypeFamilyDependencies #-}

module Michelson.Typed.EntryPoints.Error where

import Data.String
import Prelude hiding (fail, unwords, unlines, show)

import Michelson.Typed.Annotation.Path

import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Show


$(singletonsOnly [d|
  epFieldTFieldError :: Maybe Symbol -> Maybe Symbol -> Symbol
  epFieldTFieldError fieldNameA fieldNameB = unlines
       ["EpFieldT expected ",
       show_ fieldNameA,
       " but got ",
       show_ fieldNameB]

  -- epFieldRecAssertHereError :: Show a => a -> EpPath -> Symbol
  epFieldRecAssertHereError :: Show a => a -> EpPath -> Maybe Symbol -> Symbol
  epFieldRecAssertHereError t nonHere fieldAnn =
    unlines ["EpFieldRecAssertHereError ", show_ t, show_ nonHere, show_ fieldAnn]

  epFieldRecEntrypointError :: Show b => b -> EpPath -> Maybe Symbol -> Symbol -> Symbol -> Maybe Symbol -> Symbol
  epFieldRecEntrypointError ann epPath fieldName entrypointNameA entrypointNameB fieldAnn = unlines
    ["EpFieldRec _ ", show_ ann, " ",
    show_ epPath, " ", show_ fieldName,
    " _ _: expected ", entrypointNameA,
    " but got ", entrypointNameB, show_ fieldAnn]

  epFieldRecResolveOrError :: (Show a, Show b) => Symbol -> Symbol -> a -> b -> EpPath -> Maybe Symbol -> Maybe Symbol -> Symbol
  epFieldRecResolveOrError aa ab as bs nonOrEpPath fieldName fieldAnn = unlines
    ["EpFieldRecResolveOr _ _ ", show_ aa,
    " ", show_ ab,
    " ", show_ as,
    " ", show_ bs, " ",
    show_ nonOrEpPath, " ", show_ fieldName, show_ fieldAnn]

  epFieldRecResolvePairError :: (Show a, Show b) => a -> b -> EpPath -> Maybe Symbol -> Maybe Symbol -> Symbol
  epFieldRecResolvePairError as bs nonOrEpPath fieldName fieldAnn = unlines
    ["EpFieldRecResolvePair _ _ ", show_ as,
    " ", show_ bs, " ",
    show_ nonOrEpPath, " ", show_ fieldName, show_ fieldAnn]

  |])

