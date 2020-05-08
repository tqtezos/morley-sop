-- {-# LANGUAGE NoTemplateHaskell #-}
-- {-# LANGUAGE TypeFamilyDependencies #-}
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



-- -- | This fails with an error, because otherwise the @epPath@ is invalid
-- type family EpPathsAssertHere (t :: TOpq) (epPath :: EpPath) (fieldName :: Symbol) :: [EpPath] where
--   EpPathsAssertHere _ 'Here fieldName = '[fieldName]
--   EpPathsAssertHere t nonHere fieldName = '[]
--
-- singEpPathsAssertHere :: Sing t -> Sing epPath -> Sing xs -> Sing (EpPathsAssertHere t epPath xs)
-- singEpPathsAssertHere st sepPath sxs =
--       case sepPath of
--         SHere -> SCons sxs SNil
--         ((:**) _ _) -> SNil
--         ((:++) _ _) -> SNil
--
-- type family EpPathsEntrypointEq (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) (entrypointNameA :: Symbol) (entrypointNameB :: Symbol) (eqAB :: Bool) :: [EpPath] where
--   EpPathsEntrypointEq t ann epPath _ _ 'True = EpPaths t ann epPath
--   EpPathsEntrypointEq t ann epPath entrypointNameA entrypointNameB 'False = '[]
--
-- singEpPathsEntrypointEq :: forall t ann epPath entrypointNameA entrypointNameB eqAB. ()
--   => Sing t
--   -> Sing ann
--   -> Sing epPath
--   -> Sing entrypointNameA
--   -> Sing entrypointNameB
--   -> Sing eqAB
--   -> Sing (EpPathsEntrypointEq t ann epPath entrypointNameA entrypointNameB eqAB)
-- singEpPathsEntrypointEq t ann' epPath _ _ STrue =
--   singEpPaths t ann' epPath
-- singEpPathsEntrypointEq t sann epPath entrypointNameA entrypointNameB SFalse = SNil

-- type EpPathsResolveOr (ta :: TAlg) (tb :: TAlg) (aa :: Symbol) (ab :: Symbol) (as :: SymAnn ta) (bs :: SymAnn tb) = -- :: [EpPath] where
--   -- EpPathsResolveOr ta tb aa ab as bs = -- ('(:+) entrypointName epPath) =
--     ((:+@#@$$) aa <$> EpPaths ta as) ++
--     ((:+@#@$$) ab <$> EpPaths tb bs)
--
--     -- EpPathsEntrypointEq ta as epPath aa entrypointName (aa == entrypointName) ++
--     -- EpPathsEntrypointEq tb bs epPath ab entrypointName (ab == entrypointName)
--   -- EpPathsResolveOr _ _ aa ab as bs nonOrEpPath = '[]
--
-- singEpPathsResolveOr :: forall ta tb aa ab as bs. ()
--   => Sing ta
--   -> Sing tb
--   -> Sing aa
--   -> Sing ab
--   -> Sing as
--   -> Sing bs
--   -- -> Sing epPath
--   -> Sing (EpPathsResolveOr ta tb aa ab as bs) -- epPath)
-- singEpPathsResolveOr ta tb aa ab as bs =
--   withDict1 aa $
--   withDict1 ab $
--   (sing @((:+@#@$$) aa) %<$> singEpPaths ta as) %++
--   (sing @((:+@#@$$) ab) %<$> singEpPaths tb bs)
--
-- --   singEpPathsEntrypointEq ta as epPath aa entrypointName (aa %== entrypointName) %++
-- --   singEpPathsEntrypointEq tb bs epPath ab entrypointName (ab %== entrypointName)
-- -- singEpPathsResolveOr _ _ aa ab sas sbs sepPath@((:**) _ _) = SNil
-- -- singEpPathsResolveOr _ _ aa ab sas sbs sepPath@SHere = SNil
--
-- type EpPathsResolvePair (ta :: TAlg) (tb :: TAlg) (as :: SymAnn ta) (bs :: SymAnn tb) = -- :: [EpPath] where
--   -- EpPathsResolvePair ta tb as bs = -- ('(:*) epPathA epPathB) =
--     Apply (Apply (Apply LiftA2Sym0 (:*@#@$)) (EpPaths ta as)) (EpPaths tb bs)
--     -- LiftA2 (:*@#@$)
--     --   (EpPaths ta as)
--     --   (EpPaths tb bs)
--   -- EpPathsResolvePair _ _ as bs nonOrEpPath = '[]
--
-- singEpPathsResolvePair :: forall ta tb as bs. ()
--   => Sing ta
--   -> Sing tb
--   -> Sing as
--   -> Sing bs
--   -- -> Sing epPath
--   -> Sing (EpPathsResolvePair ta tb as bs) -- -- ((:**) sepPathA sepPathB) =
-- singEpPathsResolvePair sta stb sas sbs = sLiftA2 (sing @(:*@#@$))
--   (singEpPaths sta sas)
--   (singEpPaths stb sbs)
-- -- singEpPathsResolvePair _ _ sas sbs sepPath@((:++) _ _) = SNil
-- -- singEpPathsResolvePair _ _ sas sbs sepPath@SHere = SNil
--
-- -- | Note that there's no @EpPathsFieldEq@:
-- -- at that point in the resolution, there's no fieldname to check against
-- -- so we just include it in the list of results.
-- type family EpPaths (t :: TAlg) (ann :: SymAnn t) :: [EpPath] where
--   EpPaths ('TOr ta tb) ('ATOr _ aa ab as bs) = EpPathsResolveOr ta tb aa ab as bs
--   EpPaths ('TPair ta tb) ('ATPair _ _ _ as bs) = EpPathsResolvePair ta tb as bs
--   EpPaths ('TOpq t1) ('ATOpq ta) = '[ 'Here]
--
-- singEpPaths :: ()
--   => Sing t
--   -> Sing ann
--   -> Sing (EpPaths t ann)
-- singEpPaths (STOr ta tb) (SATOr _ aa ab as bs) = singEpPathsResolveOr ta tb aa ab as bs
-- singEpPaths (STPair ta tb) (SATPair _ _ _ as bs) = singEpPathsResolvePair ta tb as bs
-- singEpPaths (STOpq t1) (SATOpq ta) = sing

