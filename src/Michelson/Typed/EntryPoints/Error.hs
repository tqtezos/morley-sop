{-# OPTIONS -Wno-missing-export-lists #-}

{-# LANGUAGE TypeFamilyDependencies #-}

module Michelson.Typed.EntryPoints.Error where

import Data.String
import Prelude hiding (fail, unwords, unlines, show)

import Data.Either.Run.ErrorMessage
import Michelson.Typed.Annotation.Path

import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Show


$(singletonsOnly [d|
  epFieldTFieldError :: Symbol -> Symbol -> ErrMessage
  epFieldTFieldError fieldNameA fieldNameB = unlines
       ["EpFieldT expected ",
       fieldNameA,
       " but got ",
       fieldNameB]

  -- epFieldTAssertHereError :: Show a => a -> EpPath -> ErrMessage
  epFieldTAssertHereError :: Show a => a -> EpPath -> ErrMessage
  epFieldTAssertHereError t nonHere =
    unlines ["EpFieldTAssertHereError ", show_ t, " ", show_ nonHere]

  epFieldTEntrypointError :: Show b => b -> EpPath -> Symbol -> Symbol -> Symbol -> ErrMessage
  epFieldTEntrypointError ann epPath fieldName entrypointNameA entrypointNameB = unlines
    ["EpFieldT _ ", show_ ann, " ",
    show_ epPath, " ", fieldName,
    " _ _: expected ", entrypointNameA,
    " but got ", entrypointNameB]

  epFieldTResolveOrError :: (Show a, Show b) => Symbol -> Symbol -> a -> b -> EpPath -> Symbol -> ErrMessage
  epFieldTResolveOrError aa ab as bs nonOrEpPath fieldName = unlines
    ["EpFieldTResolveOr _ _ ", show_ aa,
    " ", show_ ab,
    " ", show_ as,
    " ", show_ bs, " ",
    show_ nonOrEpPath, " ", fieldName]

  epFieldTResolvePairError :: (Show a, Show b) => a -> b -> EpPath -> Symbol -> ErrMessage
  epFieldTResolvePairError as bs nonOrEpPath fieldName = unlines
    ["EpFieldTResolvePair _ _ ", show_ as,
    " ", show_ bs, " ",
    show_ nonOrEpPath, " ", fieldName]

  |])

