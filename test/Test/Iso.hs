{-# OPTIONS -Wno-missing-export-lists #-}

module Test.Iso where

import Prelude

import Test.QuickCheck

propIso :: (Eq a, Show a) => (a -> b) -> (b -> a) -> a -> Property
propIso to' from' = liftM2 (===) (from' . to') id

propIsoWithMiddle :: (Eq a, Show a) => (b -> String) -> (a -> b) -> (b -> a) -> a -> Property
propIsoWithMiddle middle' to' from' = liftM2 counterexample (middle' . to') $ liftM2 (===) (from' . to') id

