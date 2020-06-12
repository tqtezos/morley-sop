{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-type-patterns -Wno-orphans #-}

-- | See `Data.AltError` for documentation
module Data.AltError.TH where

import Data.Bool
import Control.Applicative
import Control.Monad
import Data.Eq
import Data.Function
import Data.String

import Control.AltError
import Control.AltError.TH
import Data.AltError

import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Show
import Data.Singletons.Prelude.Monad
import Data.Singletons.Prelude.Function
import Data.Singletons.Prelude.IsString
import Data.Singletons.TH

------------------------------------------------
-- Singletons (Symbol): Applicative and AltError
------------------------------------------------

$(singletonsOnly [d|
  caseAltE :: (Bool -> str -> r) -> (a -> r) -> AltE str a -> r
  caseAltE _ g (PureAltE x) = g x
  caseAltE f _ (AltThrow err) = f False err
  caseAltE f _ (AltExcept err) = f True err

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

  |])

