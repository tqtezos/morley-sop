{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE NoTemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Michelson.Typed.EntryPoints.Sing.Alg where

import Control.Monad
import Data.Kind
import Data.List
import Data.Functor.Classes
import Prelude hiding (unwords, unlines, show, set, fail)
import GHC.Generics ((:.:)(..))
import Text.Show

import Lorentz (Value)

import Michelson.Typed.Annotation.Path

import Michelson.Typed.T.Alg
import Michelson.Typed.Value.Free
import Michelson.Typed.EntryPoints.Sing.Alg.Types
import Michelson.Typed.EntryPoints.Sing.Alg.FieldNames
import Michelson.Typed.EntryPoints.Sing.Alg.Fields
import Michelson.Typed.EntryPoints.Sing.Alg.Paths
import Michelson.Typed.EntryPoints.Sing.Alg.Field

import Control.Lens.Setter
import Data.Singletons
import Data.Singletons.Prelude.List
import Data.Constraint

import Control.AltError
import Data.AltError
import Data.AltError.Run

import Data.Constraint.HasDict1
import Data.Singletons.WrappedSing

import Data.SOP (I(..), K(..), NP, NS)
import qualified Data.SOP as SOP


-- after conversion, likely want to expose top-layer NS, currently encoded as NP (f :.: _):
-- (f a -> m (g a)) -> NP f xs -> m (NS g xs)
newtype EpValueF (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t) where
  EpValueF :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t). ()
    => NP (EpFields f t ann) (EpPaths ann)
    -> EpValueF f t ann

deriving instance (SOP.All (SOP.Compose Show (EpFields f t ann)) (EpPaths ann)) => Show (EpValueF f t ann)

emptyEpValueF :: forall f (t :: TAlg) (ann :: SymAnn t). AltError [String] f
  => Sing t
  -> Sing ann
  -> EpValueF f t ann
emptyEpValueF st sann =
  withDict (singAllSingI $ sEpPaths sann) $
  EpValueF $
  SOP.hcpure (Proxy @SingI) $ emptyEpFields st sann sing

transEpValueF :: forall (f :: Type -> Type) (g :: Type -> Type) (t :: TAlg) (ann :: SymAnn t). ()
  => (forall t'. SingI t' => f (ValueOpq t') -> g (ValueOpq t'))
  -> Sing t
  -> Sing ann
  -> EpValueF f t ann
  -> EpValueF g t ann
transEpValueF trans' st sann (EpValueF xs) =
  withDict (singAllSingI $ sEpPaths sann) $
  EpValueF $
  SOP.hmap (transEpFields trans' st sann) xs

lensEpValueF :: forall f (t :: TAlg) (ann :: SymAnn t). (AltError [String] f)
  => Sing t
  -> Sing ann
  -> Lens' (ValueAlgT f t) (EpValueF f t ann)
lensEpValueF st sann fs xs =
  withDict (singAllSingI $ sEpPaths sann) $
  (\(EpValueF ys) -> flip appEndo xs $
    withDict (singAllSingI $ sEpPaths sann) $
    SOP.hcfoldMap
      (Proxy @SingI)
      (\zs@(EpFields sepPath _) -> Endo $
        lensEpFields st sann sepPath `set` zs
      )
      ys
  ) <$>
  fs (EpValueF $
    SOP.hcmap
      (Proxy @SingI)
      (\(WrapSing sfieldName) ->
        lensEpFields st sann sfieldName `view` xs
      ) $
    npWrappedSing $
    sEpPaths sann
  )

prfAllShow :: forall a g (xs :: [a]). (HasDict1 a, forall x. SingI x => Show (g x)) => Sing xs -> Dict (SOP.All (SOP.Compose Show g) xs)
prfAllShow SNil = Dict
prfAllShow (SCons sx sxs) =
  withDict (prfAllShow @a @g sxs) $
  withDict1 sx $
  Dict

npToNS :: forall a (f :: Type -> Type) (g :: a -> Type) (xs :: [a]). (HasDict1 a, AltError [String] f, forall x. SingI x => Show (g x))
  => Sing xs
  -> NP (f :.: g) xs
  -> f (NS g xs)
npToNS SNil SOP.Nil = altErr ["npToNS SOP.Nil"] -- fail
npToNS (SCons sx sxs) ((SOP.:*) (Comp1 xs) xss) =
  withDict1 sx $
  withDict (prfAllShow @_ @g sxs) $
  fmap SOP.Z xs <||>
  fmap SOP.S (npToNS sxs xss) -- <|>

emptyNP :: forall a (f :: Type -> Type) (g :: a -> Type) (xs :: [a]). (HasDict1 a, SingKind a, Show (Demote a), AltError [String] f, forall x. SingI x => Show (g x), SingI xs, SOP.SListI xs)
  => NP (f :.: g) xs
emptyNP =
  case SOP.sList @xs of
    SOP.SNil -> SOP.Nil
    SOP.SCons ->
      case sing @xs of
        SCons sx sxs -> withDict1 sx $ withDict1 sxs $
          Comp1 (altErr . ("emptyNP SOP.SCons: " :) . (:[]) . show . fromSing $ sing @xs) SOP.:* emptyNP -- fail
          -- altErr ["alt here?"] <||> (
          -- ) <||> altErr ["alt works!"]

nsToNP :: forall a (f :: Type -> Type) (g :: a -> Type) (xs :: [a]). (HasDict1 a, SingKind a, Show (Demote a), AltError [String] f, forall x. Show x => Show (f x), forall x. SingI x => Show (g x), SingI xs, SOP.SListI xs)
  => NS g xs
  -> NP (f :.: g) xs
nsToNP xss@(SOP.Z xs) =
  case sing @xs of
    SCons sx sxs -> withDict1 sxs $
      withDict (prfAllShow @_ @g $ sing @xs) $
      withDict (prfAllShow @_ @(f :.: g) $ sing @xs) $
      (\yss -> (trace . fromString . ("\n  nsToNP: SOP.Z\n"++) $ withDict1 sx $ show (xss, yss)) yss) $
      Comp1 (pure xs)
      SOP.:* (\yss -> (trace . fromString . ("\n  nsToNP: SOP.Z\n"++) $ withDict1 sx $ show (xss, yss)) yss) emptyNP
nsToNP xss@(SOP.S xs) =
  case sing @xs of
    SCons _ sxs -> withDict1 sxs $
      withDict (prfAllShow @_ @g $ sing @xs) $
      withDict (prfAllShow @_ @(f :.: g) $ sing @xs) $
      (\yss -> (trace . fromString . ("\n  nsToNP: SOP.S\n"++) . show $ (xss, yss)) yss ) $
      Comp1 (altErr . ("nsToNP SCons: " :) . (:[]) . show . fromSing $ sing @xs) -- fail
      SOP.:* nsToNP xs

newtype EpValue (t :: TAlg) (ann :: SymAnn t) where
  EpValue :: forall (t :: TAlg) (ann :: SymAnn t). ()
    => NS (EpFields I t ann) (EpPaths ann)
    -> EpValue t ann

deriving instance (SOP.All (SOP.Compose Show (EpFields I t ann)) (EpPaths ann)) => Show (EpValue t ann)

runEpValue :: forall f (t :: TAlg) (ann :: SymAnn t). (AltError [String] f, Monad f, Show1 f, forall x. Show x => Show (f x))
  => Sing t
  -> Sing ann
  -> EpValue t ann
  -> f (Value (FromTAlg t))
runEpValue st sann xs =
  withDict1 st $
  withDict1 sann $
  withDict (prfAllShow @_ @(EpFields f t ann) (sEpPaths sann)) $
  id $ do
    let ys = join (trace . fromString . ("\n  runEpValue:\n"++) . show) $ toEpValueF st sann xs
    zs <-
      join
        (trace . fromString . ("\n  runEpValue:\n" ++) . flip (showsPrec1 0) "") $
      runValueAlgT $
      set (lensEpValueF st sann) ys $ altErrValueAlgT ["runEpValue", show $ fromSing st] st
    return $ fromValueAlg zs

fromEpValueF :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t). AltError [String] f
  => Sing t
  -> Sing ann
  -> EpValueF f t ann
  -> f (EpValue t ann)
fromEpValueF st sann (EpValueF xs) =
  withDict (singAllSingI $ sEpPaths sann) $
  withDict1 st $
  withDict1 sann $
  EpValue <$> npToNS (sEpPaths sann) (SOP.hmap (Comp1 . unwrapEpFields st sann) xs)

prfAllShowEpFields :: forall t (ann :: SymAnn t) xs. Sing t -> Sing ann -> Sing xs -> Dict (SOP.All (SOP.Compose Show (EpFields I t ann)) xs)
prfAllShowEpFields _st _sann SNil = Dict
prfAllShowEpFields st sann (SCons _sx sxs) =
  withDict1 st $
  withDict1 sann $
  withDict (prfAllShowEpFields st sann sxs) $
  Dict

toEpValueF :: forall (f :: Type -> Type) (t :: TAlg) (ann :: SymAnn t). (AltError [String] f, Show1 f, forall x. Show x => Show (f x))
  => Sing t
  -> Sing ann
  -> EpValue t ann
  -> EpValueF f t ann
toEpValueF st sann (EpValue xs) =
  withDict (singAllSingI $ sEpPaths sann) $
  withDict1 st $ -- for tests
  withDict1 sann $ -- for tests
  withDict1 (sEpPaths sann) $
  withDict (prfAllShowEpFields st sann $ sing @(EpPaths ann)) $
  EpValueF @f @t @ann $
  SOP.hcmap (Proxy @SingI) (wrapEpFields' st sann . unComp1) $
  join (trace . fromString . ("\n  after nsToNP:\n"++) . unlines . fmap ("  "++) . SOP.hcollapse . SOP.hcmap (Proxy @SingI) (\(Comp1 ys) -> SOP.K $ showsPrec1 0 ys "")) $
  nsToNP @EpPath @f $
  join (trace . fromString . ("\n  before nsToNP:\n"++) . show) $
  xs

-- | Extract the value-level `EpPath` from an `EpValue`
epValuePath :: forall (t :: TAlg) (ann :: SymAnn t). ()
  => Sing t
  -> Sing ann
  -> EpValue t ann
  -> Path Text
epValuePath _st sann (EpValue xs) =
  withDict (singAllSingI $ sEpPaths sann) $
  SOP.hcollapse $
  SOP.hcmap (Proxy @SingI) mapper xs
  where
    mapper :: forall (epPath :: EpPath). SingI epPath
      => EpFields I t ann epPath
      -> K (Path Text) epPath
    mapper = K . const (fromSing $ sing @epPath)

-- | Extract the fields from an `EpValue`
epValueFields :: forall (t :: TAlg) (ann :: SymAnn t) r. ()
  => (forall (epPath :: EpPath). ()
    => Sing epPath
    -> NP (EpField I t ann epPath) (EpFieldNames ann epPath)
    -> r)
  -> Sing t
  -> Sing ann
  -> EpValue t ann
  -> r
epValueFields f _st sann (EpValue xs) =
  withDict (singAllSingI $ sEpPaths sann) $
  SOP.hcollapse $
  SOP.hmap mapper xs
  where
    mapper :: forall (epPath :: EpPath). ()
      => EpFields I t ann epPath
      -> K r epPath
    mapper (EpFields sepPath xss) = K $ f sepPath xss

