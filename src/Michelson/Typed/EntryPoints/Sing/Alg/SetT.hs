-- {-# LANGUAGE NoTemplateHaskell #-}
{-# OPTIONS -Wno-missing-export-lists #-}

-- | This module is a clone of @Michelson.Typed.EntryPoints.Sing.Alg.Set@,
-- where getting is replaced with setting
module Michelson.Typed.EntryPoints.Sing.Alg.SetT where

-- import Data.Either
-- import Data.Bitraversable
-- import Prelude hiding (fail, unwords, show)
-- import GHC.Generics ((:.:)(..))

-- import Data.Either.Run
-- import Data.Either.Run.ErrorMessage

-- import Michelson.Typed.Annotation.Path
-- import Michelson.Typed.EntryPoints.Error
-- import Data.Singletons.TypeLits.Util

-- import Michelson.Typed.Annotation.Sing.Alg
-- import Michelson.Typed.EntryPoints.Sing.Alg.Set (setEpFieldTFieldEq, setEpFieldTAssertHere)
-- import Michelson.Typed.T.Alg
-- import Michelson.Typed.Value.Free
-- import Michelson.Typed.EntryPoints.Sing.Alg.Types

-- import Data.Singletons
-- import Data.Singletons.TypeError
-- import Data.Singletons.Prelude.Either
-- import Data.Singletons.Prelude.Eq
-- import Data.Singletons.Prelude.Bool

-- settEpFieldTEntrypointEq :: forall f t ann epPath fieldName entrypointNameA entrypointNameB eqAB. (Alternative f, MonadFail f)
--   => Sing t
--   -> Sing ann
--   -> Sing epPath
--   -> Sing fieldName
--   -> Sing entrypointNameA
--   -> Sing entrypointNameB
--   -> Sing eqAB
--   -> RunEither SingError (f :.: ValueOpq) (EpFieldTEntrypointEq t ann epPath fieldName entrypointNameA entrypointNameB eqAB)
--   -> ValueAlgT f t
--   -> ValueAlgT f t
-- settEpFieldTEntrypointEq t ann' epPath fieldName _ _ STrue xs xss =
--   settEpFieldT t ann' epPath fieldName xs xss
-- settEpFieldTEntrypointEq t ann' epPath fieldName entrypointNameA entrypointNameB SFalse _ _ =
--   emptyValueAlgT t
--   -- Comp1 Nothing
-- -- RunLeft $ SingError $
-- --   singEpFieldTEntrypointError t ann' epPath fieldName entrypointNameA entrypointNameB

-- settEpFieldTResolveOr :: forall f ta tb aa ab as bs epPath fieldName. (Alternative f, MonadFail f)
--   => Sing ta
--   -> Sing tb
--   -> Sing aa
--   -> Sing ab
--   -> Sing as
--   -> Sing bs
--   -> Sing epPath
--   -> Sing fieldName
--   -> RunEither SingError (f :.: ValueOpq) (EpFieldTResolveOr ta tb aa ab as bs epPath fieldName)
--   -> (ValueAlgT f ta, ValueAlgT f tb)
--   -> (ValueAlgT f ta, ValueAlgT f tb)
-- settEpFieldTResolveOr ta tb aa ab as bs ((:%+) entrypointName epPath) fieldName xs xss =
--   fromMaybe xss $
--   biRunEitherAppendErrM
--     (singEpFieldTEntrypointEq ta as epPath fieldName aa entrypointName (aa %== entrypointName))
--     (singEpFieldTEntrypointEq tb bs epPath fieldName ab entrypointName (ab %== entrypointName))
--     (\ys -> settEpFieldTEntrypointEq ta as epPath fieldName aa entrypointName (aa %== entrypointName) ys `first` xss)
--     (\ys -> settEpFieldTEntrypointEq tb bs epPath fieldName ab entrypointName (ab %== entrypointName) ys `second` xss)
--     xs
-- settEpFieldTResolveOr _ _ saa sab sas sbs sepPath@((:%*) _ _) sfieldName xs xss = xss
-- --   RunLeft $ SingError $ singEpFieldTResolveOrError saa sab sas sbs sepPath sfieldName
-- settEpFieldTResolveOr _ _ saa sab sas sbs sepPath@SHere sfieldName _ xss = xss
-- --   RunLeft $ SingError $ singEpFieldTResolveOrError saa sab sas sbs sepPath sfieldName

-- settEpFieldTResolvePair :: forall f ta tb as bs epPath fieldName. (Alternative f, MonadFail f)
--   => Sing ta
--   -> Sing tb
--   -> Sing as
--   -> Sing bs
--   -> Sing epPath
--   -> Sing fieldName
--   -> RunEither SingError (f :.: ValueOpq) (EpFieldTResolvePair ta tb as bs epPath fieldName)
--   -> (ValueAlgT f ta, ValueAlgT f tb)
--   -> (ValueAlgT f ta, ValueAlgT f tb)
-- settEpFieldTResolvePair sta stb sas sbs ((:%*) sepPathA sepPathB) sfieldName xs xss =
--   fromMaybe xss $
--   biRunEitherAppendErrM
--     (singEpFieldT sta sas sepPathA sfieldName)
--     (singEpFieldT stb sbs sepPathB sfieldName)
--     (\ys -> settEpFieldT sta sas sepPathA sfieldName ys `first` xss)
--     (\ys -> settEpFieldT stb sbs sepPathB sfieldName ys `second` xss)
--     xs
-- settEpFieldTResolvePair _ _ sas sbs sepPath@((:%+) _ _) sfieldName _ xss = xss
-- settEpFieldTResolvePair _ _ sas sbs sepPath@SHere sfieldName _ xss = xss

-- settEpFieldT :: forall f t (ann :: SymAnn t) epPath fieldName. (Alternative f, MonadFail f)
--   => Sing t
--   -> Sing ann
--   -> Sing epPath
--   -> Sing fieldName
--   -> RunEither SingError (f :.: ValueOpq) (EpFieldT t ann epPath fieldName)
--   -> ValueAlgT f t
--   -> ValueAlgT f t
-- settEpFieldT (STOr ta tb) (SATOr _ aa ab as bs) epPath fieldName xs (VTOr xss) =
--   VTOr $
--   settEpFieldTResolveOr ta tb aa ab as bs epPath fieldName xs xss
-- settEpFieldT (STPair ta tb) (SATPair _ _ _ as bs) epPath fieldName xs (VTPair xss) =
--   VTPair $
--   settEpFieldTResolvePair ta tb as bs epPath fieldName xs xss
-- settEpFieldT (STOpq t1) (SATOpq ta) epPath tb xs (VTOpq xss) =
--   VTOpq $
--   unComp1 $
--   setEpFieldTFieldEq t1 (singTOpqTypeAnn ta) tb (singTOpqTypeAnn ta %== tb)
--     (setEpFieldTAssertHere t1 epPath
--       (singEpFieldTFieldEq t1 (singTOpqTypeAnn ta) tb (singTOpqTypeAnn ta %== tb))
--       xs
--     ) $
--   Comp1 $
--   xss

