{-# LANGUAGE QuantifiedConstraints #-}

module Data.AltError.Run where

import Data.Kind
import Data.List
import Data.Semigroup
import Data.Functor
import Data.Functor.Classes
import Data.Bool
import Data.Either
import Data.String
import Data.Function
import GHC.Generics
import Text.Show

import Control.AltError
import Data.AltError
import Data.Singletons.WrappedSing
import Data.Constraint.HasDict1

import Data.Singletons
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Show
import qualified Data.Text as T


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

unRunPureAltE  :: forall a (f :: [Symbol] -> Type) (g :: a -> Type) (xs :: a). RunAltE f g ('PureAltE  xs) ->   g xs
unRunPureAltE (RunPureAltE  xs) = xs

unRunAltThrow  :: forall a (f :: [Symbol] -> Type) (g :: a -> Type) (xs :: [Symbol]). RunAltE f g ('AltThrow  xs) -> f xs
unRunAltThrow (RunAltThrow  xs) = xs

unRunAltExcept :: forall a (f :: [Symbol] -> Type) (g :: a -> Type) (xs :: [Symbol]). RunAltE f g ('AltExcept xs) -> f xs
unRunAltExcept (RunAltExcept xs) = xs

coerceRunPureAltE :: forall f f' g xs. IsPureAltE xs ~ 'True => RunAltE f g xs -> RunAltE f' g xs
coerceRunPureAltE (RunPureAltE xs) = RunPureAltE xs

coerceRunImpureAltE :: forall f g g' xs. IsPureAltE xs ~ 'False => RunAltE f g xs -> RunAltE f g' xs
coerceRunImpureAltE (RunAltThrow xs) = RunAltThrow xs
coerceRunImpureAltE (RunAltExcept xs) = RunAltExcept xs

tryRunPureAltE :: forall f g (xs :: AltE [Symbol] a). Sing xs -> RunAltE f g xs -> Either String (RunAltE V1 g xs)
tryRunPureAltE sxs =
  case sIsPureAltE sxs of
    STrue -> Right . coerceRunPureAltE
    SFalse -> \_ ->
      case sxs of
        SAltThrow sys -> Left (unlines (T.unpack <$> ("tryRunPureAltE AltThrow: " : fromSing sys)))
        SAltExcept sys -> Left (unlines (T.unpack <$> ("tryRunPureAltE AltExcept: " : fromSing sys)))

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
        SPureAltE  sys -> \xss yss -> RunAltExcept $ WrapSing $ sing `SCons` sShow_ sxs `SCons` sShow_ sys `SCons` SNil
        SAltThrow  sys -> \xss yss -> RunPureAltE  $ unRunPureAltE xss
        SAltExcept sys -> \xss yss -> RunAltExcept $ WrapSing $ sys
    SAltThrow sxs ->
      \case
        SPureAltE  sys -> \xss yss -> RunPureAltE  $ unRunPureAltE yss
        SAltThrow  sys -> \xss yss -> RunAltThrow  $ WrapSing $ sing @MultipleErrors `SCons` (sUnMultipleErrors sxs %++ sUnMultipleErrors sys)
        SAltExcept sys -> \xss yss -> RunAltExcept $ WrapSing $ sing @MultipleErrors `SCons` (sUnMultipleErrors sxs %++ sUnMultipleErrors sys)
    SAltExcept sxs ->
      \case
        SPureAltE  sys -> \xss yss -> RunAltExcept $ WrapSing sxs
        SAltThrow  sys -> \xss yss -> RunAltExcept $ WrapSing $ sing @MultipleErrors `SCons` (sUnMultipleErrors sxs %++ sUnMultipleErrors sys)
        SAltExcept sys -> \xss yss -> RunAltExcept $ WrapSing $ sing @MultipleErrors `SCons` (sUnMultipleErrors sxs %++ sUnMultipleErrors sys)


parseRunAltE :: forall a (f :: [Symbol] -> Type) (g :: a -> Type) (h :: Type -> Type) (xs :: AltE [Symbol] a). (HasDict1 a, Functor h, SingI xs)
  => (forall (x :: [Symbol]). SingI x => h (f x))
  -> (forall (x :: a). SingI x => h (g x))
  -> h (RunAltE f g xs)
parseRunAltE fs gs =
  case sing @xs of
    SAltThrow ys -> RunAltThrow <$> withDict1 ys fs
    SAltExcept ys -> RunAltExcept <$> withDict1 ys fs
    SPureAltE ys -> RunPureAltE <$> withDict1 ys gs


