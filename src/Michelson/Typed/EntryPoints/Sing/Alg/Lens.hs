{-# LANGUAGE NoTemplateHaskell #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.EntryPoints.Sing.Alg.Lens where

import Control.Monad.Fail
import Prelude hiding (fail, unwords, show)
import GHC.Generics ((:.:)(..))

import Data.Either.Run
import Data.Either.Run.ErrorMessage

import Michelson.Typed.Annotation.Path
import Michelson.Typed.EntryPoints.Error

import Michelson.Typed.Annotation.Sing.Alg
import Michelson.Typed.T.Alg
import Michelson.Typed.Value.Free
import Michelson.Typed.EntryPoints.Sing.Alg.Types

import Data.Singletons
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Either
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.List
import qualified Data.Text as T

-- TODO: try to remove Alternative, currently only lensEpFieldTFieldEq depends on it


failSingValueAlgT :: forall f t (x :: Symbol). MonadFail f => Sing t -> Sing x -> ValueAlgT f t
failSingValueAlgT st = flip failValueAlgT st . ("failSingValueAlgT: " ++) . T.unpack . fromSing

lensRunEitherAppendErrM :: forall f ta tb xs ys. (MonadFail f)
  => Sing ta
  -> Sing tb
  -> Sing xs
  -> Sing ys
  -> Lens' (ValueAlgT f ta) (RunEither SingError (f :.: ValueOpq) xs)
  -> Lens' (ValueAlgT f tb) (RunEither SingError (f :.: ValueOpq) ys)
  -> Lens' (ValueAlgT f ta, ValueAlgT f tb) (RunEither SingError (f :.: ValueOpq) (xs <+> ys))
lensRunEitherAppendErrM ta tb (SLeft  sxs) (SLeft  sys) _fx _fy fs _xss =
  fmap (liftM2 (,) (failSingValueAlgT ta) (failSingValueAlgT tb) . unSingError . unRunLeft) .
  fs . RunLeft . SingError $
  sUnlines $ SCons sxs $ SCons sys SNil
lensRunEitherAppendErrM ta tb (SRight sxs) (SRight sys) _fx _fy fs _xss =
  fmap (liftM2 (,) (failSingValueAlgT ta) (failSingValueAlgT tb) . unSingError . unRunLeft) .
  fs . RunLeft . SingError $
  sEitherAppendErrMError sxs sys
lensRunEitherAppendErrM _  _  (SRight _sxs) (SLeft  _sys) fx _fy fs xss = _1 (fx fs) xss
lensRunEitherAppendErrM _  _  (SLeft  _sxs) (SRight _sys) _fx fy fs xss = _2 (fy fs) xss



lensEpFieldTFieldEq :: forall f t fieldNameA fieldNameB eqAB. (Alternative f, MonadFail f)
  => Sing t
  -> Sing fieldNameA
  -> Sing fieldNameB
  -> Sing eqAB
  -> Lens' ((f :.: ValueOpq) t) (RunEither SingError (f :.: ValueOpq) (EpFieldTFieldEq t fieldNameA fieldNameB eqAB))
lensEpFieldTFieldEq _st _ _ STrue fs xs =
  -- (fmap (Comp1 . (<|> unComp1 xs) . unComp1 . unRunRight) . fs . RunRight) xs
  (fmap (Comp1 . (unComp1 xs <|>) . unComp1 . unRunRight) . fs . RunRight) xs
lensEpFieldTFieldEq _st sfieldNameA sfieldNameB SFalse fs _ =
  Comp1 . failSingErrorLeft <$>
  (fs . RunLeft . SingError $ sEpFieldTFieldError sfieldNameA sfieldNameB)

lensEpFieldTAssertHere :: (MonadFail f)
  => Sing t
  -> Sing epPath
  -> Sing xs
  -> Lens' (RunEither SingError (f :.: ValueOpq) xs) (RunEither SingError (f :.: ValueOpq) (EpFieldTAssertHere t epPath xs))
lensEpFieldTAssertHere st sepPath sxs =
  case sxs of
    SLeft _ -> id
    SRight _ ->
      case sepPath of
        SHere -> id
        ((:%*) _ _) -> \fs _ -> RunRight . Comp1 . failSingErrorLeft <$> (fs . RunLeft . SingError $ sEpFieldTAssertHereError st sepPath)
        ((:%+) _ _) -> \fs _ -> RunRight . Comp1 . failSingErrorLeft <$> (fs . RunLeft . SingError $ sEpFieldTAssertHereError st sepPath)

lensEpFieldTEntrypointEq :: forall f t (ann :: SymAnn t) epPath fieldName entrypointNameA entrypointNameB eqAB. (Alternative f, MonadFail f)
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> Sing fieldName
  -> Sing entrypointNameA
  -> Sing entrypointNameB
  -> Sing eqAB
  -> Lens' (ValueAlgT f t) (RunEither SingError (f :.: ValueOpq) (EpFieldTEntrypointEq t ann epPath fieldName entrypointNameA entrypointNameB eqAB))
lensEpFieldTEntrypointEq t ann epPath fieldName _ _ STrue fs xs =
  lensEpFieldT t ann epPath fieldName fs xs
lensEpFieldTEntrypointEq t ann epPath fieldName entrypointNameA entrypointNameB SFalse fs _ =
  fmap (failSingValueAlgT t . unSingError . unRunLeft) . fs . RunLeft . SingError $
  sEpFieldTEntrypointError ann epPath fieldName entrypointNameA entrypointNameB

lensEpFieldTResolveOr :: forall f ta tb aa ab (as :: SymAnn ta) (bs :: SymAnn tb) epPath fieldName. (Alternative f, MonadFail f)
  => Sing ta
  -> Sing tb
  -> Sing aa
  -> Sing ab
  -> Sing as
  -> Sing bs
  -> Sing epPath
  -> Sing fieldName
  -> Lens' (ValueAlgT f ta, ValueAlgT f tb) (RunEither SingError (f :.: ValueOpq) (EpFieldTResolveOr '(ta, tb) aa ab as bs epPath fieldName))
lensEpFieldTResolveOr ta tb aa ab as bs ((:%+) entrypointName epPath) fieldName fs xs =
  lensRunEitherAppendErrM
    ta
    tb
    (sEpFieldTEntrypointEq ta as epPath fieldName aa entrypointName (aa %== entrypointName))
    (sEpFieldTEntrypointEq tb bs epPath fieldName ab entrypointName (ab %== entrypointName))
    (lensEpFieldTEntrypointEq @f ta as epPath fieldName aa entrypointName (aa %== entrypointName))
    (lensEpFieldTEntrypointEq @f tb bs epPath fieldName ab entrypointName (ab %== entrypointName))
    fs
    xs

lensEpFieldTResolveOr ta tb saa sab sas sbs sepPath@((:%*) _ _) sfieldName fs _xs =
  liftM2 (,) (failSingValueAlgT ta) (failSingValueAlgT tb) . unSingError . unRunLeft <$>
  (fs . RunLeft . SingError $ sEpFieldTResolveOrError saa sab sas sbs sepPath sfieldName)

lensEpFieldTResolveOr ta tb saa sab sas sbs sepPath@SHere sfieldName fs _xs =
  liftM2 (,) (failSingValueAlgT ta) (failSingValueAlgT tb) . unSingError . unRunLeft <$>
  (fs . RunLeft . SingError $ sEpFieldTResolveOrError saa sab sas sbs sepPath sfieldName)

lensEpFieldTResolvePair :: forall f ta tb (as :: SymAnn ta) (bs :: SymAnn tb) epPath fieldName. (Alternative f, MonadFail f)
  => Sing ta
  -> Sing tb
  -> Sing as
  -> Sing bs
  -> Sing epPath
  -> Sing fieldName
  -> Lens' (ValueAlgT f ta, ValueAlgT f tb) (RunEither SingError (f :.: ValueOpq) (EpFieldTResolvePair ta tb as bs epPath fieldName))
lensEpFieldTResolvePair sta stb sas sbs ((:%*) sepPathA sepPathB) sfieldName fs xs =
  lensRunEitherAppendErrM
    sta
    stb
    (sEpFieldT sta sas sepPathA sfieldName)
    (sEpFieldT stb sbs sepPathB sfieldName)
    (lensEpFieldT sta sas sepPathA sfieldName)
    (lensEpFieldT stb sbs sepPathB sfieldName)
    fs
    xs

lensEpFieldTResolvePair sta stb sas sbs sepPath@((:%+) _ _) sfieldName fs _xs =
  liftM2 (,) (failSingValueAlgT sta) (failSingValueAlgT stb) . unSingError . unRunLeft <$>
  (fs . RunLeft . SingError $ sEpFieldTResolvePairError sas sbs sepPath sfieldName)

lensEpFieldTResolvePair sta stb sas sbs sepPath@SHere sfieldName fs _xs =
  liftM2 (,) (failSingValueAlgT sta) (failSingValueAlgT stb) . unSingError . unRunLeft <$>
  (fs . RunLeft . SingError $ sEpFieldTResolvePairError sas sbs sepPath sfieldName)

lensEpFieldT :: forall f t (ann :: SymAnn t) epPath fieldName. (Alternative f, MonadFail f)
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> Sing fieldName
  -> Lens' (ValueAlgT f t) (RunEither SingError (f :.: ValueOpq) (EpFieldT t ann epPath fieldName))
lensEpFieldT (STOr ta tb) (SATOr _ aa ab as bs) epPath fieldName fs (VTOr xss) = VTOr <$>
  lensEpFieldTResolveOr @f ta tb aa ab as bs epPath fieldName fs xss

lensEpFieldT (STPair ta tb) (SATPair _ _ _ as bs) epPath fieldName fs (VTPair xss) = VTPair <$>
  lensEpFieldTResolvePair @f ta tb as bs epPath fieldName fs xss

lensEpFieldT (STOpq t1) (SATOpq ta) epPath tb fs (VTOpq xs) =
  VTOpq . unComp1 <$>
  (lensEpFieldTFieldEq @f t1 (sTOpqTypeAnn ta) tb (sTOpqTypeAnn ta %== tb)
    (lensEpFieldTAssertHere t1 epPath
      (sEpFieldTFieldEq t1 (sTOpqTypeAnn ta) tb (sTOpqTypeAnn ta %== tb))
      fs
    ) . Comp1
  ) xs


