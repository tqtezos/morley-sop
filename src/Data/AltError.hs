{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-type-patterns #-}

module Data.AltError where

import Data.Bool

import Control.Monad
import Data.Eq
import Text.Show
import Text.Read

-- import Control.AltError
import Data.Singletons.TypeLits

-- | `Either` with two error values:
-- - `AltThrow` for recoverable errors
-- - `AltExcept` for non-recoverable errors
data AltE str a
  = PureAltE a
  | AltThrow str
  | AltExcept str
  deriving (Eq, Read, Show)

-- | The derived instance was giving @singletons@ trouble
instance Functor (AltE str) where
  fmap f (PureAltE x) = PureAltE (f x)
  fmap _ (AltThrow x) = AltThrow x
  fmap _ (AltExcept x) = AltExcept x

-- | Type-level error monad
type ErrM = AltE [Symbol]

---------------------------
-- Applicative and AltError
---------------------------

-- | Match on `AltE`'s cases.
--
-- Note: singletons is unable to distinguish 'a' from some internal name, but
-- it's fixed by replacing @a |-> x@
caseAltE :: forall str x r. (Bool -> str -> r) -> (x -> r) -> AltE str x -> r
caseAltE _ g (PureAltE x) = g x
caseAltE f _ (AltThrow err) = f False err
caseAltE f _ (AltExcept err) = f True err

-- | Is it `PureAltE`?
isPureAltE :: forall str a. AltE str a -> Bool
isPureAltE (PureAltE _) = True
isPureAltE (AltThrow _) = False
isPureAltE (AltExcept _) = False

