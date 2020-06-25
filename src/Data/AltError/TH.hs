{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-type-patterns -Wno-orphans -Wno-redundant-constraints -Wno-unticked-promoted-constructors #-}

-- | See `Data.AltError` for documentation
module Data.AltError.TH where

import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Eq
import Data.Function
import Data.Functor.Classes
import Data.String
import Text.Read
import Text.Show
-- import Data.Int

import Control.AltError
import Control.AltError.TH
import Data.AltError
import Data.Singletons.TH.QuoteFile
import Data.Constraint.HasDict1

import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Function
import Data.Singletons.Prelude.IsString
import Data.Singletons.Prelude.Monad
import Data.Singletons.Prelude.Show
import Data.Singletons.TH
import Data.Singletons.TypeLits

------------------------------------------------
-- Singletons (Symbol): Applicative and AltError
------------------------------------------------

[hsFileSingletonsOnly|src/Data/AltError.hs|]

instance (HasDict1 str, HasDict1 a) => HasDict1 (AltE str a) where
  evidence1 = $(gen_evidence1 ''AltE)

$(singletons [d|
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

instance Show2 AltE where
  liftShowsPrec2 _ _ spA _ d (PureAltE x) = showsUnaryWith spA "PureAltE" d x
  liftShowsPrec2 spStr _ _ _ d (AltThrow x) = showsUnaryWith spStr "AltThrow" d x
  liftShowsPrec2 spStr _ _ _ d (AltExcept x) = showsUnaryWith spStr "AltExcept" d x

instance Show str => Show1 (AltE str) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

