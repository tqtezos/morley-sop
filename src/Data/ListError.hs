{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-redundant-constraints #-}

module Data.ListError where

import Data.Bool
import Data.Semigroup
import Control.Applicative
import Control.Monad
import Data.Either
import Data.Eq
import Data.Function
import Data.Functor.Classes
import Data.String
import Text.Show
import Text.Read

import Control.AltError
import Data.AltError
import Data.Constraint.HasDict1

import Data.Singletons.TH

-- | Features:
-- - @str@ exceptions and description-free errors
-- - exceptions accumulate
-- - alternation over errors/exceptions (`AltError`)
-- - `Applicative`
newtype ListE str a =
  ListE
    { unListE :: Either str [a]
    }
  deriving (Eq, Read)

$(genSingletons [''ListE])

$(singletons [d|
  deriving instance Functor (ListE str)
  |])

instance Show2 ListE where
  liftShowsPrec2 spx slx spy sly d (ListE xs) =
    showsUnaryWith (liftShowsPrec2 spx slx (liftShowsPrec spy sly) (liftShowList spy sly)) "ListE" d xs

instance Show str => Show1 (ListE str) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show str, Show a) => Show (ListE str a) where
  showsPrec = showsPrec1

instance (HasDict1 str, HasDict1 a) => HasDict1 (ListE str a) where
  evidence1 = $(gen_evidence1 ''ListE)

---------------------------
-- Applicative and ListError
---------------------------

------------------------------------------------
-- Singletons (Symbol): Applicative and ListError
------------------------------------------------

-- | Is it `pure` for `ListE`?
isPureListE :: ListE str a -> Bool
isPureListE (ListE (Right [])) = False
isPureListE (ListE (Right (_:_))) = True
isPureListE (ListE (Left _)) = False

-- | Convert `ListE` to `AltE`
listEToAltE :: forall s a. (IsString s, Eq s) => ListE [s] a -> AltE [s] [a]
listEToAltE (ListE (Left err)) = altFail err
listEToAltE (ListE (Right xs)) = pure xs

instance Applicative (ListE [s]) where
  pure = ListE . pure . pure

  (<*>) (ListE fs) (ListE xs) = ListE (liftA2 (<*>) fs xs)

instance AltError [s] (ListE [s]) where
  (<||>) (ListE (Left xs)) (ListE (Left ys)) = ListE (Left (xs <> ys))
  (<||>) (ListE (Left xs)) (ListE (Right _)) = ListE (Left xs)
  (<||>) (ListE (Right _)) (ListE (Left ys)) = ListE (Left ys)
  (<||>) (ListE (Right xs)) (ListE (Right ys)) = ListE (Right (xs <> ys))

  altErr = ListE . Right . const []
  altFail = ListE . Left

