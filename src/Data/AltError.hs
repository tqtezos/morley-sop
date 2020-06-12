{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-type-patterns #-}

module Data.AltError where

import Data.Bool
import Control.Applicative
import Control.Monad
import Data.Eq
import Data.Function
import Data.Functor.Classes
import Data.String
import Text.Show
import Text.Read

import Control.AltError
import Data.Constraint.HasDict1

import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Show
import Data.Singletons.Prelude.Monad
import Data.Singletons.TH

-- | `Either` with two error values:
-- - `AltThrow` for recoverable errors
-- - `AltExcept` for non-recoverable errors
data AltE str a
  = PureAltE a
  | AltThrow str
  | AltExcept str
  deriving (Eq, Read)

-- | Type-level error monad
type ErrM = AltE [Symbol]

$(genSingletons [''AltE])

$(singletons [d|
  deriving instance Functor (AltE str)
  |])

instance Show2 AltE where
  liftShowsPrec2 _ _ spA _ d (PureAltE x) = showsUnaryWith spA "PureAltE" d x
  liftShowsPrec2 spStr _ _ _ d (AltThrow x) = showsUnaryWith spStr "AltThrow" d x
  liftShowsPrec2 spStr _ _ _ d (AltExcept x) = showsUnaryWith spStr "AltExcept" d x

instance Show str => Show1 (AltE str) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show str, Show a) => Show (AltE str a) where
  showsPrec = showsPrec1

instance (HasDict1 str, HasDict1 a) => HasDict1 (AltE str a) where
  evidence1 = $(gen_evidence1 ''AltE)

---------------------------
-- Applicative and AltError
---------------------------

-- | Match on `AltE`'s cases
caseAltE :: (Bool -> str -> r) -> (a -> r) -> AltE str a -> r
caseAltE _ g (PureAltE x) = g x
caseAltE f _ (AltThrow err) = f False err
caseAltE f _ (AltExcept err) = f True err

-- | Is it `PureAltE`?
isPureAltE :: AltE str a -> Bool
isPureAltE (PureAltE _) = True
isPureAltE (AltThrow _) = False
isPureAltE (AltExcept _) = False

instance (IsString s, Eq s) => Applicative (AltE [s]) where
  pure = PureAltE

  (<*>) fs xs =
    caseAltE
      (\isFailFs errFs ->
        caseAltE
          (combineThrowAlt isFailFs errFs)
          (\_ -> throwAlt isFailFs errFs)
          xs
      )
      (\f -> caseAltE throwAlt (PureAltE . f) xs)
      fs


instance (IsString s, Eq s) => AltError [s] (AltE [s]) where
  (<||>) xs ys =
    caseAltE
      (\isFailXs errXs ->
        caseAltE
          (combineThrowAlt isFailXs errXs)
          (\y -> bool_
            (PureAltE y)
            (AltExcept errXs)
            isFailXs
          )
          ys
      )
      (\x ->
        caseAltE
          (\isFailYs errYs ->
            bool_
              (PureAltE x)
              (AltExcept errYs)
              isFailYs
          )
          (\y -> AltExcept [fromString "(<||>) (PureAltE _) (PureAltE _):", fromString (show_ x), fromString (show_ y)])
          ys
      )
      xs

  altErr = AltThrow
  altFail = AltExcept

instance (IsString s, Eq s) => Monad (AltE [s]) where
  (>>=) = flip (caseAltE throwAlt)

