{-# OPTIONS -Wno-missing-export-lists #-}

-- TODO:
-- - this is a port from module Michelson.Typed.EntryPoints.Sing.Alg.FieldNames where
--   we return a list of all EpPath's for an annotated TAlg
-- - Along with SOP and FieldNames, this gives a SOP implementation of Michelson
--   Value's, where intermediate transformations are performed in a single type-family pass

-- | This module is a port of Michelson.Typed.EntryPoints.Sing.Alg.Types,
-- where @ErrM@ is replaced with an accumulating @[]@
module Michelson.Typed.EntryPoints.Sing.Alg.Paths where

import Prelude

import Michelson.Typed.Annotation.Path

import Michelson.Typed.EntryPoints.Sing.Alg.Types
import Michelson.Typed.Annotation.Sing.Alg

import Data.Singletons.TH
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Functor

$(singletonsOnly [d|
  epPathsRaw :: forall t. SymAnn t -> [EpPath]
  epPathsRaw (ATOr _ aa ab as bs) =
    bool_
      ((:+) aa <$> epPathsRaw as)
      (epPathsRaw as)
      (aa == "") ++
    bool_
      ((:+) ab <$> epPathsRaw bs)
      (epPathsRaw bs)
      (ab == "")
  epPathsRaw (ATPair _ _ _ as bs) = liftA2 (:*) (epPathsRaw as) (epPathsRaw bs)
  epPathsRaw (ATOpq _ta) = [Here]

  epPaths :: forall t. SymAnn t -> [EpPath]
  epPaths ann = sort (epPathsRaw ann)

  |])

