-- {-# LANGUAGE TypeFamilyDependencies #-}

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


-- -- | This fails with an error, because otherwise the @epPath@ is invalid
-- type family EpFieldNamesAssertHere (t :: TOpq) (epPath :: EpPath) (fieldName :: Symbol) :: [Symbol] where
--   EpFieldNamesAssertHere _ 'Here fieldName = '[fieldName]
--   EpFieldNamesAssertHere t nonHere fieldName = '[]
--
-- singEpFieldNamesAssertHere :: Sing t -> Sing epPath -> Sing xs -> Sing (EpFieldNamesAssertHere t epPath xs)
-- singEpFieldNamesAssertHere st sepPath sxs =
--       case sepPath of
--         SHere -> SCons sxs SNil
--         ((:%*) _ _) -> SNil
--         ((:%+) _ _) -> SNil
--
-- type family EpFieldNamesEntrypointEq (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (entrypointNameA :: Symbol) (entrypointNameB :: Symbol) (eqAB :: Bool) :: [Symbol] where
--   EpFieldNamesEntrypointEq t ann epPath _ _ 'True = EpFieldNames t ann epPath
--   EpFieldNamesEntrypointEq t ann epPath entrypointNameA entrypointNameB 'False = '[]
--
-- singEpFieldNamesEntrypointEq :: forall t ann epPath entrypointNameA entrypointNameB eqAB. ()
--   => Sing t
--   -> Sing ann
--   -> Sing epPath
--   -> Sing entrypointNameA
--   -> Sing entrypointNameB
--   -> Sing eqAB
--   -> Sing (EpFieldNamesEntrypointEq t ann epPath entrypointNameA entrypointNameB eqAB)
-- singEpFieldNamesEntrypointEq t ann' epPath _ _ STrue =
--   singEpFieldNames t ann' epPath
-- singEpFieldNamesEntrypointEq t sann epPath entrypointNameA entrypointNameB SFalse = SNil
--
-- type family EpFieldNamesResolveOr (ta :: TAlg) (tb :: TAlg) (aa :: Symbol) (ab :: Symbol) (as :: SymAnn ta) (bs :: SymAnn tb) (epPath :: EpPath) :: [Symbol] where
--   EpFieldNamesResolveOr ta tb aa ab as bs ('(:+) entrypointName epPath) =
--     EpFieldNamesEntrypointEq ta as epPath aa entrypointName (aa == entrypointName) ++
--     EpFieldNamesEntrypointEq tb bs epPath ab entrypointName (ab == entrypointName)
--   EpFieldNamesResolveOr _ _ aa ab as bs nonOrEpPath = '[]
--
-- singEpFieldNamesResolveOr :: forall ta tb aa ab as bs epPath. ()
--   => Sing ta
--   -> Sing tb
--   -> Sing aa
--   -> Sing ab
--   -> Sing as
--   -> Sing bs
--   -> Sing epPath
--   -> Sing (EpFieldNamesResolveOr ta tb aa ab as bs epPath)
-- singEpFieldNamesResolveOr ta tb aa ab as bs ((:%+) entrypointName epPath) =
--   singEpFieldNamesEntrypointEq ta as epPath aa entrypointName (aa %== entrypointName) %++
--   singEpFieldNamesEntrypointEq tb bs epPath ab entrypointName (ab %== entrypointName)
-- singEpFieldNamesResolveOr _ _ aa ab sas sbs sepPath@((:%*) _ _) = SNil
-- singEpFieldNamesResolveOr _ _ aa ab sas sbs sepPath@SHere = SNil
--
-- type family EpFieldNamesResolvePair (ta :: TAlg) (tb :: TAlg) (as :: SymAnn ta) (bs :: SymAnn tb) (epPath :: EpPath) :: [Symbol] where
--   EpFieldNamesResolvePair ta tb as bs ('(:*) epPathA epPathB) =
--     EpFieldNames ta as epPathA ++
--     EpFieldNames tb bs epPathB
--   EpFieldNamesResolvePair _ _ as bs nonOrEpPath = '[]
--
-- singEpFieldNamesResolvePair :: forall ta tb as bs epPath. ()
--   => Sing ta
--   -> Sing tb
--   -> Sing as
--   -> Sing bs
--   -> Sing epPath
--   -> Sing (EpFieldNamesResolvePair ta tb as bs epPath)
-- singEpFieldNamesResolvePair sta stb sas sbs ((:%*) sepPathA sepPathB) =
--   singEpFieldNames sta sas sepPathA %++
--   singEpFieldNames stb sbs sepPathB
-- singEpFieldNamesResolvePair _ _ sas sbs sepPath@((:%+) _ _) = SNil
-- singEpFieldNamesResolvePair _ _ sas sbs sepPath@SHere = SNil
--
-- -- | Note that there's no @EpFieldNamesFieldEq@:
-- -- at that point in the resolution, there's no fieldname to check against
-- -- so we just include it in the list of results.
-- type family EpFieldNames (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) :: [Symbol] where
--   EpFieldNames ('TOr ta tb) ('ATOr _ aa ab as bs) epPath = EpFieldNamesResolveOr ta tb aa ab as bs epPath
--   EpFieldNames ('TPair ta tb) ('ATPair _ _ _ as bs) epPath = EpFieldNamesResolvePair ta tb as bs epPath
--   EpFieldNames ('TOpq t1) ('ATOpq ta) epPath = EpFieldNamesAssertHere t1 epPath (TOpqTypeAnn ta)
--
-- singEpFieldNames :: ()
--   => Sing t
--   -> Sing ann
--   -> Sing epPath
--   -> Sing (EpFieldNames t ann epPath)
-- singEpFieldNames (STOr ta tb) (SATOr _ aa ab as bs) epPath = singEpFieldNamesResolveOr ta tb aa ab as bs epPath
-- singEpFieldNames (STPair ta tb) (SATPair _ _ _ as bs) epPath = singEpFieldNamesResolvePair ta tb as bs epPath
-- singEpFieldNames (STOpq t1) (SATOpq ta) epPath =
--   singEpFieldNamesAssertHere t1 epPath (singTOpqTypeAnn ta)

