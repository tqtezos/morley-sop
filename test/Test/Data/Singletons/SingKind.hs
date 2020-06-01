
module Test.Data.Singletons.SingKind where

import Data.Eq
import Data.Function
import Text.Show

import Test.Iso

import Test.QuickCheck
import Data.Singletons

isoSingKind :: forall a. (SingKind a, Eq (Demote a), Show (Demote a)) => Demote a -> Property
isoSingKind = propIso toSing $ \(SomeSing sx) ->
  fromSing sx

