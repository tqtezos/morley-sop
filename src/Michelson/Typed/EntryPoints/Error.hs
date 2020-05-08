{-# OPTIONS -Wno-missing-export-lists #-}

-- {-# LANGUAGE NoTemplateHaskell #-}
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

-- type family EpFieldTFieldError (t :: a) (fieldNameA :: Symbol) (fieldNameB :: Symbol) :: ErrMessage where
--   EpFieldTFieldError t fieldNameA fieldNameB = Unlines
--     '["EpFieldT "
--      , Show_ t
--      , " expected "
--      , fieldNameA
--      , " but got "
--      , fieldNameB
--      ]
--
-- singEpFieldTFieldError :: forall a (t :: a) fieldNameA fieldNameB. SShow a
--   => Sing t
--   -> Sing fieldNameA
--   -> Sing fieldNameB
--   -> Sing (EpFieldTFieldError t fieldNameA fieldNameB)
-- singEpFieldTFieldError t fieldNameA fieldNameB = sUnlines $
--   sing @"EpFieldT " `SCons`
--   sShow_ t `SCons`
--   sing @" expected " `SCons`
--   fieldNameA `SCons`
--   sing @" but got " `SCons`
--   fieldNameB `SCons` SNil
--
--
-- type family EpFieldTAssertHereError (t :: a) (epPath :: EpPath) :: ErrMessage where
--   EpFieldTAssertHereError t nonHere =
--     Unlines '["EpFieldTAssertHereError ", Show_ t, " ", Show_ nonHere]
--
-- singEpFieldTAssertHereError :: forall a (t :: a) (epPath :: EpPath). SShow a
--   => (forall (x :: a). Sing x -> Dict (SingI x))
--   -> Sing t
--   -> Sing epPath
--   -> Sing (EpFieldTAssertHereError t epPath)
-- singEpFieldTAssertHereError singIA st sepPath =
--   withDict (singIA st) $
--   withDict (singIPath singISymbol sepPath) $
--   sUnlines $
--   sing @"EpFieldTAssertHereError " `SCons` sShow_ st `SCons` sing @" " `SCons` sShow_ sepPath `SCons` SNil
--
--
-- type family EpFieldTEntrypointError (t :: a) (ann :: b) (epPath :: EpPath) (fieldName :: Symbol) (entrypointNameA :: Symbol) (entrypointNameB :: Symbol) :: ErrMessage where
--   EpFieldTEntrypointError _ ann epPath fieldName entrypointNameA entrypointNameB = Unlines
--     '["EpFieldT _ ", Show_ ann, " ",
--     Show_ epPath, " ", fieldName,
--     " _ _: expected ", entrypointNameA,
--     " but got ", entrypointNameB]
--
-- singEpFieldTEntrypointError :: forall a b (t :: a) (ann :: b) epPath fieldName entrypointNameA entrypointNameB. (SShow b)
--   => Sing t
--   -> Sing ann
--   -> Sing epPath
--   -> Sing fieldName
--   -> Sing entrypointNameA
--   -> Sing entrypointNameB
--   -> Sing (EpFieldTEntrypointError t ann epPath fieldName entrypointNameA entrypointNameB)
-- singEpFieldTEntrypointError _ ann epPath fieldName entrypointNameA entrypointNameB = sUnlines $
--   sing @"EpFieldT _ " `SCons` sShow_ ann `SCons` sing @" " `SCons`
--   sShow_ epPath `SCons` sing @" " `SCons` fieldName `SCons`
--   sing @" _ _: expected " `SCons` entrypointNameA `SCons`
--   sing @" but got " `SCons` entrypointNameB `SCons` SNil
--
-- type family EpFieldTResolveOrError (aa :: Symbol) (ab :: Symbol) (as :: a) (bs :: b) (epPath :: EpPath) (fieldName :: Symbol) :: ErrMessage where
--   EpFieldTResolveOrError aa ab as bs nonOrEpPath fieldName = Unlines
--     '["EpFieldTResolveOr _ _ ", Show_ aa,
--     " ", Show_ ab,
--     " ", Show_ as,
--     " ", Show_ bs, " ",
--     Show_ nonOrEpPath, " ", fieldName]
--
-- singEpFieldTResolveOrError :: forall a b (aa :: Symbol) (ab :: Symbol) (as :: a) (bs :: b) (epPath :: EpPath) (fieldName :: Symbol). (SShow a, SShow b)
--   => Sing aa
--   -> Sing ab
--   -> Sing as
--   -> Sing bs
--   -> Sing epPath
--   -> Sing fieldName
--   -> Sing (EpFieldTResolveOrError aa ab as bs epPath fieldName)
-- singEpFieldTResolveOrError aa ab as bs nonOrEpPath fieldName = sUnlines $
--   sing @"EpFieldTResolveOr _ _ " `SCons` sShow_ aa `SCons`
--   sing @" " `SCons` sShow_ ab `SCons`
--   sing @" " `SCons` sShow_ as `SCons`
--   sing @" " `SCons` sShow_ bs `SCons` sing @" " `SCons`
--   sShow_ nonOrEpPath `SCons` sing @" " `SCons` fieldName `SCons` SNil
--
-- type family EpFieldTResolvePairError (as :: a) (bs :: b) (epPath :: EpPath) (fieldName :: Symbol) :: ErrMessage where
--   EpFieldTResolvePairError as bs nonOrEpPath fieldName = Unlines
--     '["EpFieldTResolvePair _ _ ", Show_ as,
--     " ", Show_ bs, " ",
--     Show_ nonOrEpPath, " ", fieldName]
--
-- singEpFieldTResolvePairError :: forall a b (as :: a) (bs :: b) epPath fieldName. (SShow a, SShow b)
--   => (forall (x :: a). Sing x -> Dict (SingI x))
--   -> (forall (x :: b). Sing x -> Dict (SingI x))
--   -> Sing as -> Sing bs -> Sing epPath -> Sing fieldName -> Sing (EpFieldTResolvePairError as bs epPath fieldName)
-- singEpFieldTResolvePairError singIA singIB sas sbs sepPath sfieldName =
--   withDict (singIA sas) $
--   withDict (singIB sbs) $
--   withDict (singIPath singISymbol sepPath) $
--   withDict (singISymbol sfieldName) $
--   sUnlines $
--     sing @"EpFieldTResolvePair _ _ " `SCons` sShow_ sas `SCons`
--     sing @" " `SCons` sShow_ sbs `SCons` sing @" " `SCons`
--     sShow_ sepPath `SCons` sing @" " `SCons` sfieldName `SCons` SNil

