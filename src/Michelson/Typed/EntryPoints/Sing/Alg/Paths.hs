{-# OPTIONS -Wno-missing-export-lists #-}

-- TODO:
-- - this is a port from module Michelson.Typed.EntryPoints.Sing.Alg.FieldNames where
--   we return a list of all EpPath's for an annotated TAlg
-- - Along with SOP and FieldNames, this gives a SOP implementation of Michelson
--   Value's, where intermediate transformations are performed in a single type-family pass

-- | This module is a port of Michelson.Typed.EntryPoints.Sing.Alg.Types,
-- where @ErrM@ is replaced with an accumulating @[]@
module Michelson.Typed.EntryPoints.Sing.Alg.Paths where

import Prelude hiding (unwords, show)

import Michelson.Typed.Annotation.Path

import Michelson.Typed.EntryPoints.Sing.Alg.Types
import Michelson.Typed.Annotation.Sing.Alg

-- import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Functor


$(singletonsOnly [d|

  epPathsResolveOr :: forall ta tb. Symbol -> Symbol -> SymAnn ta -> SymAnn tb -> [EpPath]
  epPathsResolveOr aa ab as bs =
    ((:+) aa <$> epPaths as) ++
    ((:+) ab <$> epPaths bs) -- typo: bs was as

  epPathsResolvePair :: forall ta tb. SymAnn ta -> SymAnn tb -> [EpPath]
  epPathsResolvePair as bs = liftA2 (:*) (epPaths as) (epPaths bs)

  epPaths :: forall t. SymAnn t -> [EpPath]
  epPaths (ATOr _ aa ab as bs) = epPathsResolveOr aa ab as bs
  epPaths (ATPair _ _ _ as bs) = epPathsResolvePair as bs
  epPaths (ATOpq _ta) = [Here]

  |])

