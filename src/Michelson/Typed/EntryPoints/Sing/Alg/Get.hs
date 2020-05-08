-- {-# LANGUAGE NoTemplateHaskell #-}
{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.EntryPoints.Sing.Alg.Get where

-- import Data.Either
-- import Prelude hiding (fail, unwords, show)
-- import GHC.Generics ((:.:)(..))

-- import Data.Either.Run
-- import Data.Either.Run.ErrorMessage

-- import Michelson.Typed.Annotation.Path
-- import Michelson.Typed.EntryPoints.Error
-- import Data.Singletons.TypeLits.Util

-- import Michelson.Typed.Annotation.Sing.Alg
-- import Michelson.Typed.T.Alg
-- import Michelson.Typed.Value.Free
-- import Michelson.Typed.EntryPoints.Sing.Alg.Types

-- import Data.Singletons
-- import Data.Singletons.TypeError
-- import Data.Singletons.Prelude.Either
-- import Data.Singletons.Prelude.Eq
-- import Data.Singletons.Prelude.Bool

-- getEpFieldTFieldEq :: forall t fieldNameA fieldNameB eqAB. ()
--   => Sing t
--   -> Sing fieldNameA
--   -> Sing fieldNameB
--   -> Sing eqAB
--   -> (Maybe :.: ValueOpq) t
--   -> RunEither SingError (Maybe :.: ValueOpq) (EpFieldTFieldEq t fieldNameA fieldNameB eqAB)
-- getEpFieldTFieldEq _ _ _ STrue xs = RunRight xs
-- getEpFieldTFieldEq st sfieldNameA sfieldNameB SFalse _ = RunLeft $ SingError $
--   sEpFieldTFieldError st sfieldNameA sfieldNameB

-- getEpFieldTAssertHere :: ()
--   => Sing t
--   -> Sing epPath
--   -> Sing xs
--   -> RunEither SingError (Maybe :.: ValueOpq) xs
--   -> RunEither SingError (Maybe :.: ValueOpq) (EpFieldTAssertHere t epPath xs)
-- getEpFieldTAssertHere st sepPath sxs xss =
--   case sxs of
--     SLeft sys -> RunLeft $ SingError sys
--     SRight _ ->
--       case sepPath of
--         SHere -> xss
--         ((:%*) _ _) -> RunLeft $ SingError $ singEpFieldTAssertHereError singITOpq st sepPath
--         ((:%+) _ _) -> RunLeft $ SingError $ singEpFieldTAssertHereError singITOpq st sepPath

-- getEpFieldTEntrypointEq :: forall t ann epPath fieldName entrypointNameA entrypointNameB eqAB. ()
--   => Sing t
--   -> Sing ann
--   -> Sing epPath
--   -> Sing fieldName
--   -> Sing entrypointNameA
--   -> Sing entrypointNameB
--   -> Sing eqAB
--   -> (Maybe :.: ValueAlg) t
--   -> RunEither SingError (Maybe :.: ValueOpq) (EpFieldTEntrypointEq t ann epPath fieldName entrypointNameA entrypointNameB eqAB)
-- getEpFieldTEntrypointEq t ann' epPath fieldName _ _ STrue xs =
--   getEpFieldT t ann' epPath fieldName xs
-- getEpFieldTEntrypointEq t ann' epPath fieldName entrypointNameA entrypointNameB SFalse _ = RunLeft $ SingError $
--   singEpFieldTEntrypointError t ann' epPath fieldName entrypointNameA entrypointNameB

-- getEpFieldTResolveOr :: forall ta tb aa ab as bs epPath fieldName. ()
--   => Sing ta
--   -> Sing tb
--   -> Sing aa
--   -> Sing ab
--   -> Sing as
--   -> Sing bs
--   -> Sing epPath
--   -> Sing fieldName
--   -> Either ((Maybe :.: ValueAlg) ta) ((Maybe :.: ValueAlg) tb)
--   -> RunEither SingError (Maybe :.: ValueOpq) (EpFieldTResolveOr ta tb aa ab as bs epPath fieldName)
-- getEpFieldTResolveOr ta tb aa ab as bs ((:%+) entrypointName epPath) fieldName xs =
--   runEitherAppendErrM
--     (singEpFieldTEntrypointEq ta as epPath fieldName aa entrypointName (aa %== entrypointName))
--     (singEpFieldTEntrypointEq tb bs epPath fieldName ab entrypointName (ab %== entrypointName))
--     (getEpFieldTEntrypointEq ta as epPath fieldName aa entrypointName (aa %== entrypointName) (either id (const $ Comp1 Nothing) xs))
--     (getEpFieldTEntrypointEq tb bs epPath fieldName ab entrypointName (ab %== entrypointName) (either (const $ Comp1 Nothing) id xs))
-- getEpFieldTResolveOr _ _ saa sab sas sbs sepPath@((:%*) _ _) sfieldName _ =
--   RunLeft $ SingError $ singEpFieldTResolveOrError saa sab sas sbs sepPath sfieldName
-- getEpFieldTResolveOr _ _ saa sab sas sbs sepPath@SHere sfieldName _ =
--   RunLeft $ SingError $ singEpFieldTResolveOrError saa sab sas sbs sepPath sfieldName

-- getEpFieldTResolvePair :: forall ta tb as bs epPath fieldName. ()
--   => Sing ta
--   -> Sing tb
--   -> Sing as
--   -> Sing bs
--   -> Sing epPath
--   -> Sing fieldName
--   -> (Maybe :.: ValueAlg) ta
--   -> (Maybe :.: ValueAlg) tb
--   -> RunEither SingError (Maybe :.: ValueOpq) (EpFieldTResolvePair ta tb as bs epPath fieldName)
-- getEpFieldTResolvePair sta stb sas sbs ((:%*) sepPathA sepPathB) sfieldName xs ys =
--   runEitherAppendErrM
--     (singEpFieldT sta sas sepPathA sfieldName)
--     (singEpFieldT stb sbs sepPathB sfieldName)
--     (getEpFieldT sta sas sepPathA sfieldName xs)
--     (getEpFieldT stb sbs sepPathB sfieldName ys)
-- getEpFieldTResolvePair _ _ sas sbs sepPath@((:%+) _ _) sfieldName _ _ =
--   RunLeft $ SingError $ singEpFieldTResolvePairError (singIAnnotatedAlg singISymbol) (singIAnnotatedAlg singISymbol) sas sbs sepPath sfieldName
-- getEpFieldTResolvePair _ _ sas sbs sepPath@SHere sfieldName _ _ =
--   RunLeft $ SingError $ singEpFieldTResolvePairError (singIAnnotatedAlg singISymbol) (singIAnnotatedAlg singISymbol) sas sbs sepPath sfieldName

-- getEpFieldT :: forall t (ann :: SymAnn t) epPath fieldName. ()
--   => Sing t
--   -> Sing ann
--   -> Sing epPath
--   -> Sing fieldName
--   -> (Maybe :.: ValueAlg) t
--   -> RunEither SingError (Maybe :.: ValueOpq) (EpFieldT t ann epPath fieldName)
-- getEpFieldT (STOr ta tb) (SATOr _ aa ab as bs) epPath fieldName xs =
--   case unComp1 xs of
--     Nothing ->
--       getEpFieldTResolveOr ta tb aa ab as bs epPath fieldName $ Right $ Comp1 Nothing -- `Right` picked arbitrarily
--     Just (VOr xs') ->
--       getEpFieldTResolveOr ta tb aa ab as bs epPath fieldName $ bimap (Comp1 . Just) (Comp1 . Just) xs'
-- getEpFieldT (STPair ta tb) (SATPair _ _ _ as bs) epPath fieldName xs =
--   case unComp1 xs of
--     Nothing ->
--       getEpFieldTResolvePair ta tb as bs epPath fieldName (Comp1 Nothing) (Comp1 Nothing)
--     Just (VPair (xs', ys')) ->
--       getEpFieldTResolvePair ta tb as bs epPath fieldName (Comp1 $ Just xs') (Comp1 $ Just ys')
-- getEpFieldT (STOpq t1) (SATOpq ta) epPath tb (Comp1 xs) =
--   getEpFieldTAssertHere t1 epPath
--     (singEpFieldTFieldEq t1 (singTOpqTypeAnn ta) tb (singTOpqTypeAnn ta %== tb))
--     (getEpFieldTFieldEq t1 (singTOpqTypeAnn ta) tb (singTOpqTypeAnn ta %== tb) (Comp1 $ \case { VOpq xs' -> xs' } <$> xs))

