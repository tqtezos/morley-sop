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
  epFieldTFieldError :: Symbol -> Symbol -> Symbol
  epFieldTFieldError fieldNameA fieldNameB = unlines
       ["EpFieldT expected ",
       fieldNameA,
       " but got ",
       fieldNameB]

  -- epFieldRecAssertHereError :: Show a => a -> EpPath -> Symbol
  epFieldRecAssertHereError :: Show a => a -> EpPath -> Symbol
  epFieldRecAssertHereError t nonHere =
    unlines ["EpFieldRecAssertHereError ", show_ t, " ", show_ nonHere]

  epFieldRecEntrypointError :: Show b => b -> EpPath -> Symbol -> Symbol -> Symbol -> Symbol
  epFieldRecEntrypointError ann epPath fieldName entrypointNameA entrypointNameB = unlines
    ["EpFieldRec _ ", show_ ann, " ",
    show_ epPath, " ", fieldName,
    " _ _: expected ", entrypointNameA,
    " but got ", entrypointNameB]

  epFieldRecResolveOrError :: (Show a, Show b) => Symbol -> Symbol -> a -> b -> EpPath -> Symbol -> Symbol
  epFieldRecResolveOrError aa ab as bs nonOrEpPath fieldName = unlines
    ["EpFieldRecResolveOr _ _ ", show_ aa,
    " ", show_ ab,
    " ", show_ as,
    " ", show_ bs, " ",
    show_ nonOrEpPath, " ", fieldName]

  epFieldRecResolvePairError :: (Show a, Show b) => a -> b -> EpPath -> Symbol -> Symbol
  epFieldRecResolvePairError as bs nonOrEpPath fieldName = unlines
    ["EpFieldRecResolvePair _ _ ", show_ as,
    " ", show_ bs, " ",
    show_ nonOrEpPath, " ", fieldName]

  |])

