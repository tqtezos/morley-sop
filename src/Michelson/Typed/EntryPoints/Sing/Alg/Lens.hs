{-# LANGUAGE NoTemplateHaskell #-}

-- {-# OPTIONS -fmax-pmcheck-iterations=20000000 #-} -- type-checking this module takes extra-long
{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.EntryPoints.Sing.Alg.Lens where

import Prelude hiding (unwords, unlines, show)
import GHC.Generics ((:.:)(..))
import Data.Functor.Classes

import Control.AltError
import Control.AltError.TH
import Data.AltError.TH
import Data.AltError.Run
import Data.Singletons.WrappedSing

import Michelson.Typed.Annotation.Path
import Michelson.Typed.Annotation.Sing.Alg
import Michelson.Typed.EntryPoints.Error.TH
import Michelson.Typed.EntryPoints.Sing.Alg.Types.TH
import Michelson.Typed.T.Alg
import Michelson.Typed.Value.Free

import Data.Singletons
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Applicative
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Maybe
import Data.Singletons.Prelude.Show
import Data.Singletons.Prelude.Tuple
import qualified Data.Text as T

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

(**>>) :: forall f a xs ys. AltError [String] f
  => Sing xs
  -> Sing ys
  -> Lens' a (RunSingValueOpq f ys)
  -> Lens' a (RunSingValueOpq f (xs *> ys))
(**>>) sxs sys fs gs xs =
  case sxs of
    SPureAltE _ ->
      case sys of
        SPureAltE _ -> fs gs xs
        SAltThrow _ -> fs gs xs
        SAltExcept _ -> fs gs xs
    SAltThrow sxs' ->
      case sys of
        SPureAltE _ ->
          fs
          (fmap (RunPureAltE . Comp1 . altErr . ("(**>>):" :) . fmap T.unpack . fromSing . unwrapSing . unRunAltThrow) .
            gs . RunAltThrow . WrapSing . const sxs' . unRunPureAltE
          )
          xs
        SAltThrow sys' ->
          fs
          (fmap (RunAltThrow . const (WrapSing sys') . unRunAltThrow) .
            gs . RunAltThrow . WrapSing . sMergeErrors sxs' . unwrapSing . unRunAltThrow
          )
          xs
        SAltExcept sys' ->
          fs
          (fmap (RunAltExcept . const (WrapSing sys') . unRunAltExcept) .
            gs . RunAltExcept . WrapSing . sMergeErrors sxs' . unwrapSing . unRunAltExcept
          )
          xs
    SAltExcept sxs' ->
      case sys of
        SPureAltE _ ->
          fs
          (fmap (RunPureAltE . Comp1 . altFail . ("(**>>):" :) . fmap T.unpack . fromSing . unwrapSing . unRunAltExcept) .
            gs . RunAltExcept . WrapSing . const sxs' . unRunPureAltE
          )
          xs
        SAltThrow sys' ->
          fs
          (fmap (RunAltThrow . const (WrapSing sys') . unRunAltExcept) .
            gs . RunAltExcept . WrapSing . sMergeErrors sxs' . unwrapSing . unRunAltThrow
          )
          xs
        SAltExcept sys' ->
          fs
          (fmap (RunAltExcept . const (WrapSing sys') . unRunAltExcept) .
            gs . RunAltExcept . WrapSing . sMergeErrors sxs' . unwrapSing . unRunAltExcept
          )
          xs

lensEpFieldTResolveOr :: forall f ta tb aa ab (as :: SymAnn ta) (bs :: SymAnn tb) epPath fieldName fieldAnn. (Show1 f, AltError [String] f)
  => Sing ta
  -> Sing tb
  -> Sing aa
  -> Sing ab
  -> Sing as
  -> Sing bs
  -> Sing epPath
  -> Sing fieldName
  -> Sing fieldAnn
  -> Lens' (ValueAlgT f ta, ValueAlgT f tb) (RunSingValueOpq f (EpFieldTResolveOr '(ta, tb) '(aa, ab) as bs epPath fieldName fieldAnn))
lensEpFieldTResolveOr ta tb aa ab as bs ((:%+) entrypointName epPath) fieldName fieldAnn fs xs =
  lensRunAltEAppendErrM
    ta
    tb
      (sBool_
        (sAltErr (sEpFieldRecEntrypointError as epPath fieldName aa entrypointName fieldAnn `SCons` SNil))
        (sEpFieldT' ta as epPath fieldName (SJust aa))
        (aa %== entrypointName)
      )
      (sBool_
        (sAltErr (sEpFieldRecEntrypointError bs epPath fieldName ab entrypointName fieldAnn `SCons` SNil))
        (sEpFieldT' tb bs epPath fieldName (SJust ab))
        (ab %== entrypointName)
      )
    (\gs ys ->
      case aa %== entrypointName of
        STrue ->
          (lensEpFieldT' ta as epPath fieldName (SJust aa)) gs ys
        SFalse ->
          fmap (flip altErrValueAlgT ta . fromUnwrapSing . unRunAltThrow) . gs . RunAltThrow . WrapSing $
          sEpFieldRecEntrypointError as epPath fieldName aa entrypointName fieldAnn `SCons` SNil
    )
    (\gs ys ->
      case ab %== entrypointName of
        STrue ->
          (lensEpFieldT' tb bs epPath fieldName (SJust ab)) gs ys
        SFalse ->
          fmap (flip altErrValueAlgT tb . fromUnwrapSing . unRunAltThrow) . gs . RunAltThrow . WrapSing $
          sEpFieldRecEntrypointError bs epPath fieldName ab entrypointName fieldAnn `SCons` SNil
    )
    fs
    xs

lensEpFieldTResolveOr ta tb saa sab sas sbs sepPath@((:%*) _ _) sfieldName sfieldAnn fs _xs =
  liftM2 (,) (flip altFailValueAlgT ta) (flip altFailValueAlgT tb) . fromUnwrapSing . unRunAltExcept <$>
  (fs . RunAltExcept . WrapSing . flip SCons SNil $ sEpFieldRecResolveOrError saa sab sas sbs sepPath sfieldName sfieldAnn)

lensEpFieldTResolveOr ta tb saa sab sas sbs sepPath@SHere sfieldName sfieldAnn fs _xs =
  liftM2 (,) (flip altFailValueAlgT ta) (flip altFailValueAlgT tb) . fromUnwrapSing . unRunAltExcept <$>
  (fs . RunAltExcept . WrapSing . flip SCons SNil $ sEpFieldRecResolveOrError saa sab sas sbs sepPath sfieldName sfieldAnn)

lensEpFieldTResolvePair :: forall f ta tb aa ab (as :: SymAnn ta) (bs :: SymAnn tb) epPath fieldName fieldAnn. (Show1 f, AltError [String] f)
  => Sing ta
  -> Sing tb
  -> Sing aa
  -> Sing ab
  -> Sing as
  -> Sing bs
  -> Sing epPath
  -> Sing fieldName
  -> Sing fieldAnn
  -> Lens' (ValueAlgT f ta, ValueAlgT f tb) (RunSingValueOpq f (EpFieldTResolvePair '(ta, tb) '(aa, ab) as bs epPath fieldName fieldAnn))
lensEpFieldTResolvePair sta stb saa sab sas sbs ((:%*) sepPathA sepPathB) sfieldName _sfieldAnn fs xs =
  lensRunAltEAppendErrM
    sta
    stb
    (sEpFieldT' sta sas sepPathA sfieldName (SJust saa))
    (sEpFieldT' stb sbs sepPathB sfieldName (SJust sab))
    (lensEpFieldT' sta sas sepPathA sfieldName (SJust saa))
    (lensEpFieldT' stb sbs sepPathB sfieldName (SJust sab))
    fs
    xs

lensEpFieldTResolvePair sta stb _saa _sab sas sbs sepPath@((:%+) _ _) sfieldName sfieldAnn fs _xs =
  liftM2 (,) (flip altFailValueAlgT sta) (flip altFailValueAlgT stb) . fromUnwrapSing . unRunAltExcept <$>
  (fs . RunAltExcept . WrapSing . flip SCons SNil $ sEpFieldRecResolvePairError sas sbs sepPath sfieldName sfieldAnn)

lensEpFieldTResolvePair sta stb _saa _sab sas sbs sepPath@SHere sfieldName sfieldAnn fs _xs =
  liftM2 (,) (flip altFailValueAlgT sta) (flip altFailValueAlgT stb) . fromUnwrapSing . unRunAltExcept <$>
  (fs . RunAltExcept . WrapSing . flip SCons SNil $ sEpFieldRecResolvePairError sas sbs sepPath sfieldName sfieldAnn)

lensEpFieldT' :: forall f t (ann :: SymAnn t) epPath fieldName (fieldAnn :: Maybe Symbol). (Show1 f, AltError [String] f)
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> Sing fieldName
  -> Sing fieldAnn
  -> Lens' (ValueAlgT f t) (RunSingValueOpq f (EpFieldT' t ann epPath fieldName fieldAnn))
lensEpFieldT' (STOr ta tb) (SATOr _ aa ab as bs) epPath fieldName fieldAnn fs (VTOr xss) = VTOr <$>
  lensEpFieldTResolveOr @f ta tb aa ab as bs epPath fieldName fieldAnn fs xss
lensEpFieldT' (STPair ta tb) (SATPair _ aa ab as bs) epPath fieldName fieldAnn fs (VTPair xss) = VTPair <$>
  lensEpFieldTResolvePair @f ta tb aa ab as bs epPath fieldName fieldAnn fs xss
lensEpFieldT' (STOpq t1) (SATOpq _ta) epPath tb fieldAnn fs (VTOpq xs) =
  VTOpq . (<||>) xs <$>
  (**>>)
  (sBool_ (sAltFail (sEpFieldRecAssertHereError t1 epPath fieldAnn `SCons` SNil)) (sPure STuple0) (epPath %== SHere))
  (sEpFieldRecT tb t1 fieldAnn)
  (case tb %== fieldAnn of
     SFalse -> \f _ys ->
       fmap (altErr . fmap T.unpack . fromSing . unwrapSing . unRunAltThrow) .
       f . RunAltThrow . WrapSing $
       sEpFieldTFieldError tb fieldAnn `SCons` SNil
     STrue -> \f ->
       fmap (unComp1 . unRunPureAltE) . f . RunPureAltE . Comp1
  )
  fs
  xs

lensEpFieldT :: forall f t (ann :: SymAnn t) epPath fieldName. (Show1 f, AltError [String] f)
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> Sing fieldName
  -> Lens' (ValueAlgT f t) (RunSingValueOpq f (EpFieldT t ann epPath fieldName))
lensEpFieldT t ann epPath fieldName =
  lensEpFieldT' t ann epPath fieldName SNothing

