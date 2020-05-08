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


-- | Combine errors and error if both `Right`
-- type family (<+>) (xs :: ErrM a) (ys :: ErrM a) :: ErrM a where
--   (<+>) ('Left errA) ('Left errB) = 'Left (Unlines '[errA, errB])
--   (<+>) ('Right xs) ('Right ys) = 'Left (Unlines '["(<+>) ('Right ", Show_ xs, ") ('Right ", Show_ ys, ")"])
--   (<+>) ('Right xs) _ = 'Right xs
--   (<+>) _ ('Right ys) = 'Right ys

-- -- | Lift @(`<+>`)@ over `Sing`
-- singEitherAppendErrM :: forall a (xs :: ErrM a) (ys :: ErrM a). SShow a => Sing xs -> Sing ys -> Sing (xs <+> ys)
-- singEitherAppendErrM (SLeft errA) (SLeft errB) = SLeft $
--   sUnlines $ SCons errA $ SCons errB SNil
-- singEitherAppendErrM (SRight xs) (SRight ys) = SLeft $
--   sUnlines $ SCons (sing @"(<+>) ('Right ") $ SCons (sShow_ xs) $ SCons (sing @") ('Right ") $ SCons (sShow_ ys) $ SCons (sing @")") SNil
-- singEitherAppendErrM (SRight xs) (SLeft _) = SRight xs
-- singEitherAppendErrM (SLeft _) (SRight ys) = SRight ys

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

-- -- | "Reverse" of `runEitherAppendErrM`
-- biRunEitherAppendErrM :: forall f xs ys r. ()
--   => Sing xs
--   -> Sing ys
--   -> (RunEither SingError f xs -> r)
--   -> (RunEither SingError f ys -> r)
--   -> RunEither SingError f (xs <+> ys)
--   -> Maybe r
-- biRunEitherAppendErrM (SLeft  _sxs) (SLeft  _sys) _fx _fy _xss = Nothing
-- biRunEitherAppendErrM (SRight _sxs) (SRight _sys) _fx _fy _xss = Nothing
-- biRunEitherAppendErrM (SRight _sxs) (SLeft  _sys) fx _fy xss = Just $ fx xss
-- biRunEitherAppendErrM (SLeft  _sxs) (SRight _sys) _fx fy xss = Just $ fy xss


-- class Functor f => EmptyChoice f where
--   emptyChoice :: forall a r. (forall x. Proxy x -> f x -> r) -> f a -> r
--    :: forall g a. Functor g => f (g (

-- instance EmptyChoice

-- lensRunEitherAppendErrM :: forall a f g (xs :: ErrM a) (ys :: ErrM a) s t. (SShow a, MonadFail f)
--   => Sing xs
--   -> Sing ys
--   -> Lens' (f s) (RunEither SingError (f :.: g) xs)
--   -> Lens' (f t) (RunEither SingError (f :.: g) ys)
--   -> Lens' (f s, f t) (RunEither SingError (f :.: g) (xs <+> ys))
-- -- lensRunEitherAppendErrM sxs sys fs gs hs xss =
--   -- _ fs gs hs xss

-- lensRunEitherAppendErrM (SLeft  sxs) (SLeft  sys) fx fy fs xss = undefined -- fmap failSingErrorLeft . fs . RunLeft . SingError $
--   -- sUnlines $ SCons sxs $ SCons sys SNil
-- lensRunEitherAppendErrM (SRight sxs) (SRight sys) fx fy fs xss = undefined -- fmap failSingErrorLeft . fs . RunLeft . SingError $
--   -- sEitherAppendErrMError sxs sys
-- lensRunEitherAppendErrM (SRight sxs) (SLeft _sys) fx fy fs xss = _ (fx fs) xss -- id $ _1 (fx fs) $ xss
-- lensRunEitherAppendErrM (SLeft _sxs) (SRight sys) fx fy fs xss = traverse (fy fs) xss -- id $ _2 (fy fs) $ xss

-- _1 :: Lens (a,b) (a',b) a a'
-- _2 :: Lens (a, b) (a, b') b b'

-- _Left :: Prism (Either a c) (Either b c) a b
-- _Right :: Prism (Either c a) (Either c b) a b


-- Homogenous:

-- _1 :: Lens' (a, b) a
-- _2 :: Lens' (a, b) b

-- _Left  :: Prism' (Either a b) a
-- _Right :: Prism' (Either b a) a

-- failableLens :: MonadFail f => Prism s a -> Lens' s (f a)

-- LensLike' f s a -> LensLike' f t b -> LensLike' f (p s t) (

--         _ :: Sing (EpFieldT ta as n fieldName)
--              -> Sing (EpFieldT tb bs n1 fieldName)
--              -> ((RunEither
--                     SingError (f2 :.: ValueOpq) (EpFieldT ta as n fieldName)
--                   -> f4 (RunEither
--                            SingError (f2 :.: ValueOpq) (EpFieldT ta as n fieldName)))
--                  -> ValueAlgT f2 ta -> f4 (ValueAlgT f2 ta))
--              -> ((RunEither
--                     SingError (f3 :.: ValueOpq) (EpFieldT tb bs n1 fieldName)
--                   -> f5 (RunEither
--                            SingError (f3 :.: ValueOpq) (EpFieldT tb bs n1 fieldName)))
--                  -> ValueAlgT f3 tb -> f5 (ValueAlgT f3 tb))
--              -> f6 (ValueAlgT f ta, ValueAlgT f tb)

--         _ :: Sing ta
--              -> Sing tb
--              -> Sing aa
--              -> Sing ab
--              -> Sing as
--              -> Sing bs
--              -> Sing n
--              -> Sing n1
--              -> Sing fieldName
--              -> (RunEither SingError (f :.: ValueOpq) (EpFieldTEntrypointEq ta as n1 fieldName aa n (DefaultEq aa n) <+> EpFieldTEntrypointEq tb bs n1 fieldName ab n (DefaultEq ab n))
--                  -> f6 (RunEither SingError (f :.: ValueOpq) (EpFieldTEntrypointEq ta as n1 fieldName aa n (DefaultEq aa n) <+> EpFieldTEntrypointEq
--                                  tb bs n1 fieldName ab n (DefaultEq ab n))))
--              -> (ValueAlgT f ta, ValueAlgT f tb)
--              -> Sing (EpFieldTEntrypointEq ta as n1 fieldName aa n (DefaultEq aa n))
--              -> Sing (EpFieldTEntrypointEq tb bs n1 fieldName ab n (DefaultEq ab n))
--              -> ((RunEither SingError (f :.: ValueOpq) (EpFieldTEntrypointEq ta as n1 fieldName aa n (DefaultEq aa n))
--                   -> f0 (RunEither SingError (f :.: ValueOpq) (EpFieldTEntrypointEq ta as n1 fieldName aa n (DefaultEq aa n))))
--                  -> ValueAlgT f ta
--                  -> f0 (ValueAlgT f ta))
--              -> ((RunEither SingError (f :.: ValueOpq) (EpFieldTEntrypointEq tb bs n1 fieldName ab n (DefaultEq ab n))
--                   -> f1 (RunEither SingError (f :.: ValueOpq) (EpFieldTEntrypointEq tb bs n1 fieldName ab n (DefaultEq ab n))))
--                  -> ValueAlgT f tb -> f1 (ValueAlgT f tb))

