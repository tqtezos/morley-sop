{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Data.AltError.Run where

import Data.Kind
import Data.Functor
import Data.Functor.Classes
import Data.String
import Data.Function
import Text.Show

-- import Control.AltError
import Control.AltError.TH
import Data.AltError
import Data.AltError.TH
import Data.Singletons.WrappedSing
import Data.Constraint.HasDict1

import Data.Singletons
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Show


-- | An embedding of @`AltE` `[Symbol]`@
data RunAltE (f :: [Symbol] -> Type) (g :: a -> Type) (xs :: AltE [Symbol] a) where
  RunPureAltE  :: forall a (f :: [Symbol] -> Type) (g :: a -> Type) (xs :: a).   g xs -> RunAltE f g ('PureAltE  xs)
  RunAltThrow  :: forall a (f :: [Symbol] -> Type) (g :: a -> Type) (xs :: [Symbol]). f xs -> RunAltE f g ('AltThrow  xs)
  RunAltExcept :: forall a (f :: [Symbol] -> Type) (g :: a -> Type) (xs :: [Symbol]). f xs -> RunAltE f g ('AltExcept xs)

instance forall a (f :: [Symbol] -> Type) (g :: a -> Type) (xs :: AltE [Symbol] a). (HasDict1 a, forall (x :: [Symbol]). SingI x => Show (f x), forall (y :: a). SingI y => Show (g y), SingI xs) => Show (RunAltE f g xs) where
  showsPrec d (RunAltThrow xs) =
    case sing @xs of
      SAltThrow sxs -> withDict1 sxs $ showsUnaryWith showsPrec "RunAltThrow" d xs
  showsPrec d (RunAltExcept xs) =
    case sing @xs of
      SAltExcept sxs -> withDict1 sxs $ showsUnaryWith showsPrec "RunAltExcept" d xs
  showsPrec d (RunPureAltE xs) =
    case sing @xs of
      SPureAltE sxs -> withDict1 sxs $ showsUnaryWith showsPrec "RunPureAltE" d xs

-- | Unwrap `RunPureAltE`
unRunPureAltE  :: forall a (f :: [Symbol] -> Type) (g :: a -> Type) (xs :: a). RunAltE f g ('PureAltE  xs) ->   g xs
unRunPureAltE (RunPureAltE  xs) = xs

-- | Unwrap `RunAltThrow`
unRunAltThrow  :: forall a (f :: [Symbol] -> Type) (g :: a -> Type) (xs :: [Symbol]). RunAltE f g ('AltThrow  xs) -> f xs
unRunAltThrow (RunAltThrow  xs) = xs

-- | Unwrap `RunAltExcept`
unRunAltExcept :: forall a (f :: [Symbol] -> Type) (g :: a -> Type) (xs :: [Symbol]). RunAltE f g ('AltExcept xs) -> f xs
unRunAltExcept (RunAltExcept xs) = xs

-- | Lift @(`<+>`)@ over `RunEither`
runAltEAlt :: forall a f (xs :: AltE [Symbol] a) (ys :: AltE [Symbol] a). SShow a
  => Sing xs
  -> Sing ys
  -> RunAltE WrappedSing f xs
  -> RunAltE WrappedSing f ys
  -> RunAltE WrappedSing f (xs <||> ys)
runAltEAlt =
  \case
    SPureAltE sxs ->
      \case
        SPureAltE   sys -> \_xss _yss -> RunAltExcept $ WrapSing $ sing `SCons` sShow_ sxs `SCons` sShow_ sys `SCons` SNil
        SAltThrow  _sys -> \ xss _yss -> RunPureAltE  $ unRunPureAltE xss
        SAltExcept  sys -> \_xss _yss -> RunAltExcept $ WrapSing $ sys
    SAltThrow sxs ->
      \case
        SPureAltE  _sys -> \_xss  yss -> RunPureAltE  $ unRunPureAltE yss
        SAltThrow   sys -> \_xss _yss -> RunAltThrow  $ WrapSing $ sing @MultipleErrors `SCons` (sUnMultipleErrors sxs %++ sUnMultipleErrors sys)
        SAltExcept  sys -> \_xss _yss -> RunAltExcept $ WrapSing $ sing @MultipleErrors `SCons` (sUnMultipleErrors sxs %++ sUnMultipleErrors sys)
    SAltExcept sxs ->
      \case
        SPureAltE  _sys -> \_xss _yss -> RunAltExcept $ WrapSing sxs
        SAltThrow   sys -> \_xss _yss -> RunAltExcept $ WrapSing $ sing @MultipleErrors `SCons` (sUnMultipleErrors sxs %++ sUnMultipleErrors sys)
        SAltExcept  sys -> \_xss _yss -> RunAltExcept $ WrapSing $ sing @MultipleErrors `SCons` (sUnMultipleErrors sxs %++ sUnMultipleErrors sys)

-- | Parse `RunAltE`, given parsers for @[`Symbol`]@ and @a@
parseRunAltE :: forall a (f :: [Symbol] -> Type) (g :: a -> Type) (h :: Type -> Type) (xs :: AltE [Symbol] a). (HasDict1 a, Functor h)
  => (forall (x :: [Symbol]). SingI x => h (f x))
  -> (forall (x :: a). SingI x => h (g x))
  -> Sing xs
  -> h (RunAltE f g xs)
parseRunAltE fs gs sxs =
  case sxs of
    SAltThrow ys -> RunAltThrow <$> withDict1 ys fs
    SAltExcept ys -> RunAltExcept <$> withDict1 ys fs
    SPureAltE ys -> RunPureAltE <$> withDict1 ys gs

-- | Given a method to fuse @f@ and @g@
-- when @g@ is applied to a `Sing` value,
-- embed @f@ in `RunAltE`
wrapRunAltE :: forall f g g' xs. Functor f
  => (forall x. Sing x -> f (g x) -> g' x)
  -> Sing xs
  -> f (RunAltE WrappedSing g xs)
  -> RunAltE WrappedSing g' xs
wrapRunAltE fs sxs xss =
  case sxs of
    SAltThrow sys -> RunAltThrow $ WrapSing sys
    SAltExcept sys -> RunAltExcept $ WrapSing sys
    SPureAltE sys -> RunPureAltE $ fs sys $ unRunPureAltE <$> xss

-- | Given methods to produce @f, g@, resp.,
-- produce a `RunAltE` given any `Sing`
pureRunAltE :: forall a (f :: [Symbol] -> Type) (g :: a -> Type) (xs :: AltE [Symbol] a). ()
  => (forall (x :: [Symbol]). Sing x -> f x)
  -> (forall (x :: a). Sing x -> g x)
  -> Sing xs
  -> RunAltE f g xs
pureRunAltE fs gs sxs =
  case sxs of
    SAltThrow sys -> RunAltThrow $ fs sys
    SAltExcept sys -> RunAltExcept $ fs sys
    SPureAltE sys -> RunPureAltE $ gs sys

