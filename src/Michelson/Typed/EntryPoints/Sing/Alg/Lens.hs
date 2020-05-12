{-# LANGUAGE NoTemplateHaskell #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.EntryPoints.Sing.Alg.Lens where

import Prelude hiding (unwords, unlines, show)
import GHC.Generics ((:.:)(..))

import Control.AltError
import Data.AltError
import Data.AltError.Run
-- import Data.Either.Run
-- import Data.Either.Run.ErrorMessage

import Michelson.Typed.Annotation.Path
import Michelson.Typed.EntryPoints.Error

import Michelson.Typed.Annotation.Sing.Alg
import Michelson.Typed.T.Alg
import Michelson.Typed.Value.Free
import Michelson.Typed.EntryPoints.Sing.Alg.Types
import Data.Singletons.WrappedSing

import Data.Singletons
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Show
import qualified Data.Text as T

-- TODO: try to remove Alternative, currently only lensEpFieldTFieldEq depends on it

-- tt = _

-- | `unwrapSing` then `fromSing`, with `Text` to `String` conversion
fromUnwrapSing :: forall (x :: [Symbol]). WrappedSing x -> [String]
fromUnwrapSing = fmap (fromString . T.unpack) . fromSing . unwrapSing

type RunSingValueOpq f = RunAltE WrappedSing (f :.: ValueOpq)

lensRunAltEAppendErrM :: forall f ta tb xs ys. AltError [String] f
  => Sing ta
  -> Sing tb
  -> Sing xs
  -> Sing ys
  -> Lens' (ValueAlgT f ta) (RunSingValueOpq f xs)
  -> Lens' (ValueAlgT f tb) (RunSingValueOpq f ys)
  -> Lens' (ValueAlgT f ta, ValueAlgT f tb) (RunSingValueOpq f (xs <||> ys))
lensRunAltEAppendErrM ta tb sxs sys fx fy fs xss =
  case sxs of
    SAltExcept sxs' ->
      case sys of
        SAltExcept sys' ->
          fmap (liftM2 (,) (flip altFailValueAlgT ta) (flip altFailValueAlgT tb) . fromUnwrapSing . unRunAltExcept) $
          fs . RunAltExcept . WrapSing $ sMergeErrors sxs' sys'
        SAltThrow sys' ->
          fmap (liftM2 (,) (flip altFailValueAlgT ta) (flip altFailValueAlgT tb) . fromUnwrapSing . unRunAltExcept) $
          fs . RunAltExcept . WrapSing $ sMergeErrors sxs' sys'
        SPureAltE _ ->
          fmap (liftM2 (,) (flip altFailValueAlgT ta) (flip altFailValueAlgT tb) . fromUnwrapSing . unRunAltExcept) $
          fs . RunAltExcept . WrapSing $ sxs'
    SAltThrow sxs' ->
      case sys of
        SAltExcept sys' ->
          fmap (liftM2 (,) (flip altFailValueAlgT ta) (flip altFailValueAlgT tb) . fromUnwrapSing . unRunAltExcept) $
          fs . RunAltExcept . WrapSing $ sMergeErrors sxs' sys'
        SAltThrow sys' ->
          fmap (liftM2 (,) (flip altErrValueAlgT ta) (flip altErrValueAlgT tb) . fromUnwrapSing . unRunAltThrow) $
          fs . RunAltThrow . WrapSing $ sMergeErrors sxs' sys'
        SPureAltE _ -> _2 (fy fs) xss
    SPureAltE sxs' ->
      case sys of
        SAltExcept sys' ->
          fmap (liftM2 (,) (flip altFailValueAlgT ta) (flip altFailValueAlgT tb) . fromUnwrapSing . unRunAltExcept) $
          fs . RunAltExcept . WrapSing $ sys'
        SAltThrow _ -> _1 (fx fs) xss
        SPureAltE sys' ->
          fmap (liftM2 (,) (flip altFailValueAlgT ta) (flip altFailValueAlgT tb) . fromUnwrapSing . unRunAltExcept) $
          fs . RunAltExcept . WrapSing $
          sing @"(<||>) (PureAltE _) (PureAltE _):" `SCons` sShow_ sxs' `SCons` sShow_ sys' `SCons` SNil

lensEpFieldTFieldEq :: forall f t fieldNameA fieldNameB eqAB. AltError [String] f
  => Sing t
  -> Sing fieldNameA
  -> Sing fieldNameB
  -> Sing eqAB
  -> Lens' ((f :.: ValueOpq) t) (RunSingValueOpq f (EpFieldTFieldEq t fieldNameA fieldNameB eqAB))
lensEpFieldTFieldEq _st _ _ STrue fs xs =
  -- (fmap (Comp1 . (<|> unComp1 xs) . unComp1 . unRunRight) . fs . RunPureAltE) xs
  (fmap unRunPureAltE . fs . RunPureAltE) xs
lensEpFieldTFieldEq _st sfieldNameA sfieldNameB SFalse fs _ =
  Comp1 . altErr . fromUnwrapSing . unRunAltThrow <$>
  (fs . RunAltThrow . WrapSing . flip SCons SNil $ sEpFieldTFieldError sfieldNameA sfieldNameB)

lensEpFieldTAssertHere :: AltError [String] f
  => Sing t
  -> Sing epPath
  -> Sing xs
  -> Lens' (RunSingValueOpq f xs) (RunSingValueOpq f (EpFieldTAssertHere t epPath xs))
lensEpFieldTAssertHere st sepPath sxs =
  case sxs of
    SAltThrow _ -> id
    SAltExcept _ -> id
    SPureAltE _ ->
      case sepPath of
        SHere -> id
        ((:%*) _ _) -> \fs _ -> RunPureAltE . Comp1 . altFail . fromUnwrapSing . unRunAltExcept <$> (fs . RunAltExcept . WrapSing . flip SCons SNil $ sEpFieldTAssertHereError st sepPath)
        ((:%+) _ _) -> \fs _ -> RunPureAltE . Comp1 . altFail . fromUnwrapSing . unRunAltExcept <$> (fs . RunAltExcept . WrapSing . flip SCons SNil $ sEpFieldTAssertHereError st sepPath)

lensEpFieldTEntrypointEq :: forall f t (ann :: SymAnn t) epPath fieldName entrypointNameA entrypointNameB eqAB. AltError [String] f
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> Sing fieldName
  -> Sing entrypointNameA
  -> Sing entrypointNameB
  -> Sing eqAB
  -> Lens' (ValueAlgT f t) (RunSingValueOpq f (EpFieldTEntrypointEq t ann epPath fieldName entrypointNameA entrypointNameB eqAB))
lensEpFieldTEntrypointEq t ann epPath fieldName _ _ STrue fs xs =
  lensEpFieldT t ann epPath fieldName fs xs
lensEpFieldTEntrypointEq t ann epPath fieldName entrypointNameA entrypointNameB SFalse fs _ =
  fmap (flip altErrValueAlgT t . fromUnwrapSing . unRunAltThrow) . fs . RunAltThrow . WrapSing . flip SCons SNil $
  sEpFieldTEntrypointError ann epPath fieldName entrypointNameA entrypointNameB

lensEpFieldTResolveOr :: forall f ta tb aa ab (as :: SymAnn ta) (bs :: SymAnn tb) epPath fieldName. AltError [String] f
  => Sing ta
  -> Sing tb
  -> Sing aa
  -> Sing ab
  -> Sing as
  -> Sing bs
  -> Sing epPath
  -> Sing fieldName
  -> Lens' (ValueAlgT f ta, ValueAlgT f tb) (RunSingValueOpq f (EpFieldTResolveOr '(ta, tb) aa ab as bs epPath fieldName))
lensEpFieldTResolveOr ta tb aa ab as bs ((:%+) entrypointName epPath) fieldName fs xs =
  lensRunAltEAppendErrM
    ta
    tb
    (sEpFieldTEntrypointEq ta as epPath fieldName aa entrypointName (aa %== entrypointName))
    (sEpFieldTEntrypointEq tb bs epPath fieldName ab entrypointName (ab %== entrypointName))
    (lensEpFieldTEntrypointEq @f ta as epPath fieldName aa entrypointName (aa %== entrypointName))
    (lensEpFieldTEntrypointEq @f tb bs epPath fieldName ab entrypointName (ab %== entrypointName))
    fs
    xs

lensEpFieldTResolveOr ta tb saa sab sas sbs sepPath@((:%*) _ _) sfieldName fs _xs =
  liftM2 (,) (flip altFailValueAlgT ta) (flip altFailValueAlgT tb) . fromUnwrapSing . unRunAltExcept <$>
  (fs . RunAltExcept . WrapSing . flip SCons SNil $ sEpFieldTResolveOrError saa sab sas sbs sepPath sfieldName)

lensEpFieldTResolveOr ta tb saa sab sas sbs sepPath@SHere sfieldName fs _xs =
  liftM2 (,) (flip altFailValueAlgT ta) (flip altFailValueAlgT tb) . fromUnwrapSing . unRunAltExcept <$>
  (fs . RunAltExcept . WrapSing . flip SCons SNil $ sEpFieldTResolveOrError saa sab sas sbs sepPath sfieldName)

lensEpFieldTResolvePair :: forall f ta tb (as :: SymAnn ta) (bs :: SymAnn tb) epPath fieldName. AltError [String] f
  => Sing ta
  -> Sing tb
  -> Sing as
  -> Sing bs
  -> Sing epPath
  -> Sing fieldName
  -> Lens' (ValueAlgT f ta, ValueAlgT f tb) (RunSingValueOpq f (EpFieldTResolvePair ta tb as bs epPath fieldName))
lensEpFieldTResolvePair sta stb sas sbs ((:%*) sepPathA sepPathB) sfieldName fs xs =
  lensRunAltEAppendErrM
    sta
    stb
    (sEpFieldT sta sas sepPathA sfieldName)
    (sEpFieldT stb sbs sepPathB sfieldName)
    (lensEpFieldT sta sas sepPathA sfieldName)
    (lensEpFieldT stb sbs sepPathB sfieldName)
    fs
    xs

lensEpFieldTResolvePair sta stb sas sbs sepPath@((:%+) _ _) sfieldName fs _xs =
  liftM2 (,) (flip altFailValueAlgT sta) (flip altFailValueAlgT stb) . fromUnwrapSing . unRunAltExcept <$>
  (fs . RunAltExcept . WrapSing . flip SCons SNil $ sEpFieldTResolvePairError sas sbs sepPath sfieldName)

lensEpFieldTResolvePair sta stb sas sbs sepPath@SHere sfieldName fs _xs =
  liftM2 (,) (flip altFailValueAlgT sta) (flip altFailValueAlgT stb) . fromUnwrapSing . unRunAltExcept <$>
  (fs . RunAltExcept . WrapSing . flip SCons SNil $ sEpFieldTResolvePairError sas sbs sepPath sfieldName)

lensEpFieldT :: forall f t (ann :: SymAnn t) epPath fieldName. AltError [String] f
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> Sing fieldName
  -> Lens' (ValueAlgT f t) (RunSingValueOpq f (EpFieldT t ann epPath fieldName))
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


