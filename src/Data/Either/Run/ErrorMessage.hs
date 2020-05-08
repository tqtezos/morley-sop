{-# OPTIONS -Wno-missing-export-lists -Wno-orphans #-}

module Data.Either.Run.ErrorMessage where

import Data.Either
import Control.Monad.Fail
import Prelude hiding (fail, unwords, show)
import Text.Show

import Data.Either.Run

import qualified Data.Text as T
import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.TypeError
import Data.Singletons.Prelude.Either
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Show

failSing :: forall f (x :: Symbol) a. MonadFail f => Sing x -> f a
failSing = fail . ("failSing: " ++) . T.unpack . fromSing

failSingError :: forall f (x :: Symbol) a. MonadFail f => SingError x -> f a
failSingError = failSing . unSingError

failSingErrorLeft :: forall f g (x :: Symbol) a. MonadFail f => RunEither SingError g ('Left x) -> f a
failSingErrorLeft = failSingError . unRunLeft


-- | `showErrorMessage`
instance Show ErrorMessage where show = showErrorMessage

-- | A type-level error message
type ErrMessage = Symbol -- ErrorMessage' Symbol

-- | Wrapped `Sing` for `ErrorMessage'` `Symbol`
newtype SingError (xs :: ErrMessage) where
  SingError :: { unSingError :: Sing xs } -> SingError xs
  deriving Show

-- | Type-level error monad
type ErrM = Either ErrMessage

$(singletonsOnly [d|
  eitherAppendErrMError :: Show a => a -> a -> Symbol
  eitherAppendErrMError xs ys = unlines ["(<+>) (Right ", show_ xs, ") (Right ", show_ ys, ")"]

  (<+>) :: Show a => ErrM a -> ErrM a -> ErrM a
  (<+>) (Left  errA) (Left  errB) = Left (unlines [errA, errB])
  (<+>) (Right xs  ) (Right ys  ) = Left (eitherAppendErrMError xs ys)
  (<+>) (Right xs  ) (Left  _   ) = Right xs
  (<+>) (Left  _   ) (Right ys  ) = Right ys

  |])

-- | Lift @(`<+>`)@ over `RunEither`
runEitherAppendErrM :: forall a f (xs :: ErrM a) (ys :: ErrM a). SShow a
  => Sing xs
  -> Sing ys
  -> RunEither SingError f xs
  -> RunEither SingError f ys
  -> RunEither SingError f (xs <+> ys)
runEitherAppendErrM _ _ (RunLeft (SingError serrA)) (RunLeft (SingError serrB)) = RunLeft $ SingError $
  sUnlines $ SCons serrA $ SCons serrB SNil
runEitherAppendErrM sxs sys (RunRight _) (RunRight _) =
  case (sxs, sys) of
    (SRight sxs', SRight sys') -> RunLeft $ SingError $
      sEitherAppendErrMError sxs' sys'
runEitherAppendErrM _ _ (RunRight sxs) (RunLeft _) = RunRight sxs
runEitherAppendErrM _ _ (RunLeft _) (RunRight sys) = RunRight sys

