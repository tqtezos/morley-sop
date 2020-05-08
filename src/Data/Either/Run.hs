{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS -Wno-missing-export-lists #-}

module Data.Either.Run where

import Data.Kind
import Data.Functor.Classes
import Data.Either
import Text.Show
import Data.String
import Data.Function

import Data.Constraint.HasDict1

import Data.Singletons
import Data.Singletons.Prelude.Either

-- | Given handlers for `Left` and `Right`, handle `Either`
data RunEither (f :: a -> Type) (g :: b -> Type) (xs :: Either a b) where
  RunLeft :: f x -> RunEither f g ('Left x)
  RunRight :: g y -> RunEither f g ('Right y)

instance forall a b (f :: a -> Type) (g :: b -> Type) (xs :: Either a b). (HasDict1 a, HasDict1 b, forall (x :: a). SingI x => Show (f x), forall (y :: b). SingI y => Show (g y), SingI xs) => Show (RunEither f g xs) where
  showsPrec d (RunLeft xs) =
    case sing @xs of
      SLeft sxs -> withDict1 sxs $ showsUnaryWith showsPrec "RunLeft" d xs
  showsPrec d (RunRight xs) =
    case sing @xs of
      SRight sxs -> withDict1 sxs $ showsUnaryWith showsPrec "RunRight" d xs

-- | Unwrap `RunLeft`
unRunLeft :: forall a b (f :: a -> Type) (g :: b -> Type) (x :: a). ()
  => RunEither f g ('Left x)
  -> f x
unRunLeft (RunLeft xs) = xs

-- | Unwrap `RunRight`
unRunRight :: forall a b (f :: a -> Type) (g :: b -> Type) (y :: b). ()
  => RunEither f g ('Right y)
  -> g y
unRunRight (RunRight xs) = xs

