{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.EntryPoints.Sing.Alg.Fields where

import Data.Kind
import Data.Functor.Classes
import Text.Show
import Data.String
import GHC.Generics ((:.:)(..))
import Prelude hiding (Map, All, unwords, show, set, unlines)

import Control.AltError
import Data.AltError
import Data.AltError.Run

import Michelson.Typed.Annotation.Path

import Michelson.Typed.T.Alg
import Michelson.Typed.Value.Free
import Michelson.Typed.EntryPoints.Sing.Alg.Types
import Michelson.Typed.EntryPoints.Sing.Alg.Field
import Michelson.Typed.EntryPoints.Sing.Alg.Lens
import Data.Constraint.HasDict1
import Data.Singletons.WrappedSing

import Control.Lens.Setter
import Data.SOP (I(..), NP)
import qualified Data.SOP as SOP
import Data.Singletons
import Data.Singletons.Prelude.List
import Data.Constraint

trace3 :: String -> a -> a
trace3 = flip const -- trace . fromString -- flip const

data EpFields (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) where
  EpFields :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). ()
    => Sing epPath
    -> RunAltE WrappedSing (NP (EpField f t ann epPath)) (EpFieldNamesErrM t ann epPath)
    -> EpFields f t ann epPath

prfAllShowEpField :: forall f t (ann :: SymAnn t) epPath xs. (forall t'. SingI t' => Show (f (ValueOpq t')))
  => Sing t -> Sing ann -> Sing epPath -> Sing xs -> Dict (SOP.All (SOP.Compose Show (EpField f t ann epPath)) xs)
prfAllShowEpField _st _sann _sepPath SNil = Dict
prfAllShowEpField st sann sepPath (SCons _sx sxs) =
  withDict1 st $
  withDict1 sann $
  withDict1 sepPath $
  withDict (prfAllShowEpField @f st sann sepPath sxs) $
  Dict

instance forall f t (ann :: SymAnn t) (epPath :: EpPath). (forall t'. SingI t' => Show (f (ValueOpq t')), SingI t, SingI ann) => Show (EpFields f t ann epPath) where
  showsPrec d (EpFields sepPath (RunAltThrow xs)) =
    showsBinaryWith showsPrec showsPrec "EpFields" d (fromSing sepPath) xs
  showsPrec d (EpFields sepPath (RunAltExcept xs)) =
    showsBinaryWith showsPrec showsPrec "EpFields" d (fromSing sepPath) xs
  showsPrec d (EpFields sepPath (RunPureAltE xs)) =
    case sEpFieldNamesErrM (sing @t) (sing @ann) sepPath of
      SPureAltE sxs ->
        withDict (prfAllShowEpField @f (sing @t) (sing @ann) sepPath sxs) $
        showsBinaryWith showsPrec showsPrec "EpFields" d (fromSing sepPath) xs

unwrapEpFields :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). Applicative f
  => Sing t
  -> Sing ann
  -> EpFields f t ann epPath
  -> f (EpFields I t ann epPath)
unwrapEpFields _st _sann (EpFields sepPath (RunAltThrow xs)) =
  EpFields sepPath . RunAltThrow <$> pure xs
unwrapEpFields _st _sann (EpFields sepPath (RunAltExcept xs)) =
  EpFields sepPath . RunAltExcept <$> pure xs
unwrapEpFields st sann (EpFields sepPath (RunPureAltE xs)) =
  case sEpFieldNamesErrM st sann sepPath of
    SPureAltE sxs ->
      withDict (singAllSingI sxs) $
      EpFields sepPath . RunPureAltE <$>
      SOP.htraverse' unwrapEpField xs

-- | Generalization from the following:
-- @
--  (\xs -> (fmap fst xs, fmap snd xs)) :: Functor f => f (a, b) -> (f a, f b)
-- @
--
-- to `NP`
wrapNP :: forall a (f :: Type -> Type) (g :: a -> Type) (xs :: [a]). (Show1 f, Functor f, SOP.SListI xs)
  => f (NP g xs)
  -> NP (f :.: g) xs
wrapNP xss =
  case SOP.sList @xs of
    SOP.SNil -> SOP.Nil -- warning: throws away nil case, i.e. (f ())
    SOP.SCons ->
        Comp1
        (\case { (SOP.:*) ys _ -> ys } <$> xss) SOP.:*
        wrapNP
        (\case { (SOP.:*) _ yss -> yss } <$> xss)

wrapEpFields :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). (Show1 f, Functor f, SingI epPath)
  => Sing t
  -> Sing ann
  -> f (EpFields I t ann epPath)
  -> EpFields f t ann epPath
wrapEpFields st sann xs =
  EpFields @f @t @ann @epPath (sing @epPath) $
  wrapRunAltE
    (\sys ->
      withDict (singAllSingI sys) $
      SOP.hcmap (Proxy @SingI) (wrapEpField st sann . unComp1) .
      wrapNP
    )
    (sEpFieldNamesErrM st sann (sing @epPath)) $
  (\case { EpFields _ xss -> xss }) <$> xs

emptyEpFields :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). AltError [String] f
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> EpFields f t ann epPath
emptyEpFields st sann sepPath =
  EpFields sepPath $
  singToRunAltE
    WrapSing
    (\sxs ->
      withDict (singAllSingI sxs) $
      SOP.hcpure (Proxy @SingI) $ emptyEpField st sann sepPath sing
    )
    (sEpFieldNamesErrM st sann sepPath)

transEpFields :: forall (f :: Type -> Type) (g :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). ()
  => (forall t'. SingI t' => f (ValueOpq t') -> g (ValueOpq t'))
  -> Sing t
  -> Sing ann
  -> EpFields f t ann epPath
  -> EpFields g t ann epPath
transEpFields trans' st sann (EpFields sepPath xs) =
  EpFields sepPath $
  case xs of
    RunAltThrow ys -> RunAltThrow ys
    RunAltExcept ys -> RunAltExcept ys
    RunPureAltE ys -> RunPureAltE $
      case sEpFieldNamesErrM st sann sepPath of
        SPureAltE sxs ->
          withDict (singAllSingI sxs) $
          SOP.hmap (transEpField trans' st sann sepPath) ys

lensEpFields :: forall f (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). (AltError [String] f, Show1 f)
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> Lens' (ValueAlgT f t) (EpFields f t ann epPath)
lensEpFields st sann sepPath fs xs =
  withDict1 st $
  withDict1 sann $
  withDict1 sepPath $
  (\(EpFields _ ys) ->
    case ys of
      RunAltThrow zs -> altErrValueAlgT (fromUnwrapSing zs) st
      RunAltExcept zs -> altFailValueAlgT (fromUnwrapSing zs) st
      RunPureAltE zs ->
        case sEpFieldNamesErrM (sing @t) (sing @ann) sepPath of
          SPureAltE sxs ->
            withDict (singAllSingI sxs) $
            flip appEndo xs $
            SOP.hcfoldMap
              (Proxy @SingI)
              (\(EpField sfieldName ws) -> Endo $
                \ss ->
                  trace3
                    (unlines
                       [ "epPath"
                       , fromString . show $ fromSing sepPath
                       , "fieldName"
                       , fromString . show $ fromSing sfieldName
                       , "before"
                       , fromString $ show ss
                       , "after"
                       , fromString $
                         show (((lensEpFieldT st sann sepPath sfieldName) `set` ws) ss)
                       ]) $
                  ((lensEpFieldT st sann sepPath sfieldName) `set` ws) ss
              )
              zs
  ) <$>
  (fs . EpFields sepPath $
    singToRunAltE
      WrapSing
      (\sxs ->
        withDict (singAllSingI sxs) $
        SOP.hcmap
          (Proxy @SingI)
          (\(WrapSing sfieldName) ->
            EpField @f @t @ann @epPath sfieldName $
            lensEpFieldT st sann sepPath sfieldName `view` xs
          ) $
        npWrappedSing $
        sxs
      )
      (sEpFieldNamesErrM st sann sepPath)
  )

