{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.EntryPoints.Sing.Alg.Fields where

import Data.Kind
import Data.Functor.Classes
import Text.Show
import GHC.Generics ((:.:)(..))
import Prelude hiding (All, unwords, show, set)

import Data.SOP.Map
import qualified Data.SOP.Map as SOP

import Michelson.Typed.Annotation.Path

import Michelson.Typed.T.Alg
import Michelson.Typed.Value.Free
import Michelson.Typed.EntryPoints.Sing.Alg.Types
import Michelson.Typed.EntryPoints.Sing.Alg.FieldNames
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


type EpFieldTs (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) = SOP.Map (EpFieldTSym t ann epPath) (EpFieldNames ann epPath)

singEpFieldTs :: forall (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). (SingI t, SingI ann, SingI epPath)
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> Sing (EpFieldTs t ann epPath)
singEpFieldTs _st sann sepPath =
  singMap @(EpFieldTSym t ann epPath) $
  sEpFieldNames sann sepPath

data EpFields (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath) where
  EpFields :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). ()
    => Sing epPath
    -> NP (EpField f t ann epPath) (EpFieldNames ann epPath)
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
  showsPrec d (EpFields sepPath xs) =
    withDict (prfAllShowEpField @f (sing @t) (sing @ann) sepPath (sEpFieldNames (sing @ann) sepPath)) $
    showsBinaryWith showsPrec showsPrec "EpFields" d (fromSing sepPath) xs


unwrapEpFields :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). Applicative f
  => Sing t
  -> Sing ann
  -> EpFields f t ann epPath
  -> f (EpFields I t ann epPath)
unwrapEpFields _st sann (EpFields sepPath xs) =
  withDict (singAllSingI $ sEpFieldNames sann sepPath) $
  EpFields sepPath <$>
  SOP.htraverse' unwrapEpField xs

-- wrapEpFields :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). Applicative f
--   => Sing t
--   -> Sing ann
--   -> EpFields I t ann epPath
--   -> EpFields f t ann epPath
-- wrapEpFields _st sann (EpFields sepPath xs) =
--   withDict (singAllSingI $ sEpFieldNames sann sepPath) $
--   EpFields sepPath $
--   SOP.hmap wrapEpField xs


-- | Generalization from the following:
-- @
--  (\xs -> (fmap fst xs, fmap snd xs)) :: Functor f => f (a, b) -> (f a, f b)
-- @
--
-- to `NP`
wrapNP :: forall a (f :: Type -> Type) (g :: a -> Type) (xs :: [a]). (Functor f, SOP.SListI xs)
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

wrapEpFields' :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). (Functor f, SingI epPath)
  => Sing t
  -> Sing ann
  -> f (EpFields I t ann epPath)
  -> EpFields f t ann epPath
wrapEpFields' st sann xs =
  EpFields (sing @epPath) $
  withDict (singAllSingI $ sEpFieldNames sann (sing @epPath)) $
  SOP.hcmap (Proxy @SingI) (wrapEpField' st sann . unComp1) $
  wrapNP $
  (\case { EpFields _ xss -> xss }) <$> xs

emptyEpFields :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). MonadFail f
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> EpFields f t ann epPath
emptyEpFields st sann sepPath =
  withDict (singAllSingI $ sEpFieldNames sann sepPath) $
  EpFields sepPath $
  SOP.hcpure (Proxy @SingI) $ emptyEpField st sann sepPath sing


transEpFields :: forall (f :: Type -> Type) (g :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). ()
  => (forall t'. SingI t' => f (ValueOpq t') -> g (ValueOpq t'))
  -> Sing t
  -> Sing ann
  -> EpFields f t ann epPath
  -> EpFields g t ann epPath
transEpFields trans' st sann (EpFields sepPath xs) =
  withDict (singAllSingI $ sEpFieldNames sann sepPath) $
  EpFields sepPath $
  SOP.hmap (transEpField trans' st sann sepPath) xs

lensEpFields :: forall f (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). (Alternative f, MonadFail f)
  => Sing t
  -> Sing ann
  -> Sing epPath
  -> Lens' (ValueAlgT f t) (EpFields f t ann epPath)
lensEpFields st sann sepPath fs xs =
  withDict1 st $
  withDict1 sann $
  withDict1 sepPath $
  withDict (singAllSingI $ sEpFieldNames sann sepPath) $
  (\(EpFields _ ys) -> flip appEndo xs $
    SOP.hcfoldMap
      (Proxy @SingI)
      (\(EpField sfieldName zs) -> Endo $
        (lensEpFieldT st sann sepPath sfieldName) `set` zs
      )
      ys
  ) <$>
  (fs . EpFields sepPath $
    SOP.hcmap
      (Proxy @SingI)
      (\(WrapSing sfieldName) ->
        EpField @f @t @ann @epPath sfieldName $
        lensEpFieldT st sann sepPath sfieldName `view` xs
      ) $
    npWrappedSing $
    sEpFieldNames sann sepPath
  )

-- setEpFields :: forall f (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). (MonadFail f)
--   => Sing t
--   -> Sing ann
--   -> Sing epPath
--   -> EpFields f t ann epPath
--   -> ValueAlg t
--   -> f (ValueAlg t)
-- setEpFields st sann sepPath (EpFields _ xs) xss =
--   unComp1 $
--   flip appEndo (Comp1 $ return xss) $
--   withDict1 st $
--   withDict1 sann $
--   withDict1 sepPath $
--   withDict (singAllSingI $ sEpFieldNames sann sepPath) $
--   SOP.hcfoldMap
--     (Proxy @SingI)
--     (\(EpField sfieldName ys) -> Endo $ \yss ->
--       setEpFieldT st sann sepPath sfieldName ys yss
--     )
--     xs

-- settEpFields :: forall f (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). (Alternative f, MonadFail f)
--   => Sing t
--   -> Sing ann
--   -> Sing epPath
--   -> EpFields f t ann epPath
--   -> ValueAlgT f t
--   -> ValueAlgT f t
-- settEpFields st sann sepPath (EpFields _ xs) xss =
--   flip appEndo xss $
--   withDict1 st $
--   withDict1 sann $
--   withDict1 sepPath $
--   withDict (singAllSingI $ sEpFieldNames sann sepPath) $
--   SOP.hcfoldMap
--     (Proxy @SingI)
--     (\(EpField sfieldName ys) -> Endo $ \yss ->
--       settEpFieldT st sann sepPath sfieldName ys yss
--     )
--     xs

-- getEpFields :: forall (t :: TAlg) (ann :: SymAnn t) (epPath :: EpPath). ()
--   => Sing t
--   -> Sing ann
--   -> Sing epPath
--   -> ValueAlg t
--   -> EpFields Maybe t ann epPath
-- getEpFields st sann sepPath xss =
--   withDict (singAllSingI $ sEpFieldNames sann sepPath) $
--   EpFields sepPath $
--   SOP.hcmap
--     (Proxy @SingI)
--     (\(WrapSing sfieldName) ->
--       EpField sfieldName $ getEpFieldT st sann sepPath sfieldName $ Comp1 . return $ xss
--     ) $
--   npWrappedSing $
--   sEpFieldNames sann sepPath

