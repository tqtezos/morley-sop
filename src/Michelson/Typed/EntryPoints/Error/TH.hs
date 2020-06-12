{-# OPTIONS -Wno-missing-export-lists #-}

{-# LANGUAGE TypeFamilyDependencies #-}

-- | See `Michelson.Typed.EntryPoints.Error` for documentation
module Michelson.Typed.EntryPoints.Error.TH where

import Data.String
import Prelude hiding (fail, unwords, unlines, show)

import Michelson.Typed.Annotation.Path

import Data.Singletons.TH
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Show
import Data.Singletons.Prelude.IsString


$(singletonsOnly [d|

  epFieldTFieldError :: forall s. (IsString s, Show s) => Maybe s -> Maybe s -> s
  epFieldTFieldError fieldNameA fieldNameB = fromString (unlines
       ["EpFieldT expected ",
       show_ fieldNameA,
       " but got ",
       show_ fieldNameB])

  -- epFieldRecAssertHereError :: Show a => a -> EpPath -> s
  epFieldRecAssertHereError :: forall s a. (IsString s, Show s, Show a) => a -> Path s -> Maybe s -> s
  epFieldRecAssertHereError t nonHere fieldAnn = fromString (unlines
    ["EpFieldRecAssertHereError ", show_ t, show_ nonHere, show_ fieldAnn])

  epFieldRecEntrypointError :: forall s a. (IsString s, Show s, Show a) => a -> Path s -> Maybe s -> s -> s -> Maybe s -> s
  epFieldRecEntrypointError ann epPath fieldName entrypointNameA entrypointNameB fieldAnn = fromString (unlines
    ["EpFieldRec _ ", show_ ann, " ",
    show_ epPath, " ", show_ fieldName,
    " _ _: expected ", show_ entrypointNameA,
    " but got ", show_ entrypointNameB, show_ fieldAnn])

  epFieldRecResolveOrError :: forall s a b. (IsString s, Show s, Show a, Show b) => s -> s -> a -> b -> Path s -> Maybe s -> Maybe s -> s
  epFieldRecResolveOrError aa ab as bs nonOrEpPath fieldName fieldAnn = fromString (unlines
    ["EpFieldRecResolveOr _ _ ", show_ aa,
    " ", show_ ab,
    " ", show_ as,
    " ", show_ bs, " ",
    show_ nonOrEpPath, " ", show_ fieldName, show_ fieldAnn])

  epFieldRecResolvePairError :: forall s a b. (IsString s, Show s, Show a, Show b) => a -> b -> Path s -> Maybe s -> Maybe s -> s
  epFieldRecResolvePairError as bs nonOrEpPath fieldName fieldAnn = fromString (unlines
    ["EpFieldRecResolvePair _ _ ", show_ as,
    " ", show_ bs, " ",
    show_ nonOrEpPath, " ", show_ fieldName, show_ fieldAnn])

  |])


