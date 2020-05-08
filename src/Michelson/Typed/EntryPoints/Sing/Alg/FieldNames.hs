{-# OPTIONS -Wno-missing-export-lists #-}

-- | This module is a port of Michelson.Typed.EntryPoints.Sing.Alg.Types,
-- where @ErrM@ is replaced with an accumulating @[]@
module Michelson.Typed.EntryPoints.Sing.Alg.FieldNames where

import Prelude hiding (unwords, show)

import Michelson.Typed.Annotation.Path
import Michelson.Typed.EntryPoints.Sing.Alg.Types
import Michelson.Typed.Annotation.Sing.Alg

import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.List

$(singletonsOnly [d|

  epFieldNamesAssertHere :: EpPath -> Symbol -> [Symbol]
  epFieldNamesAssertHere Here fieldName = [fieldName]
  epFieldNamesAssertHere ((:*) _ _) _fieldName = []
  epFieldNamesAssertHere ((:+) _ _) _fieldName = []

  epFieldNamesEntrypointEq :: forall t. SymAnn t -> EpPath -> Symbol -> Symbol -> Bool -> [Symbol]
  epFieldNamesEntrypointEq ann epPath _ _ True = epFieldNames ann epPath
  epFieldNamesEntrypointEq _ann _epPath _entrypointNameA _entrypointNameB False = []

  epFieldNamesResolveOr :: forall ta tb. Symbol -> Symbol -> SymAnn ta -> SymAnn tb -> EpPath -> [Symbol]
  epFieldNamesResolveOr aa ab as bs ((:+) entrypointName epPath) =
    epFieldNamesEntrypointEq as epPath aa entrypointName (aa == entrypointName) ++
    epFieldNamesEntrypointEq bs epPath ab entrypointName (ab == entrypointName)
  epFieldNamesResolveOr _aa _ab _as _bs ((:*) _ _) = []
  epFieldNamesResolveOr _aa _ab _as _bs Here = []

  epFieldNamesResolvePair :: forall ta tb. SymAnn ta -> SymAnn tb -> EpPath -> [Symbol]
  epFieldNamesResolvePair as bs ((:*) epPathA epPathB) =
    epFieldNames as epPathA ++
    epFieldNames bs epPathB
  epFieldNamesResolvePair _as _bs ((:+) _ _) = []
  epFieldNamesResolvePair _as _bs Here = []

  -- | Note that theres no @epFieldNamesFieldEq@:
  -- at that point in the resolution, theres no fieldname to check against
  -- so we just include it in the list of results.
  epFieldNames :: forall t. SymAnn t -> EpPath -> [Symbol]
  epFieldNames (ATOr _ aa ab as bs) epPath = epFieldNamesResolveOr aa ab as bs epPath
  epFieldNames (ATPair _ _ _ as bs) epPath = epFieldNamesResolvePair as bs epPath
  epFieldNames (ATOpq ta) epPath = epFieldNamesAssertHere epPath (tOpqTypeAnn ta)

  |])

