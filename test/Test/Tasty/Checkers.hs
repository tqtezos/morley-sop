{-# OPTIONS -Wno-missing-export-lists #-}

module Test.Tasty.Checkers where

import Data.Functor
import Data.Function
import Data.Tuple

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck.Checkers

testBatch :: TestBatch -> TestTree
testBatch = uncurry testGroup . fmap (fmap (uncurry testProperty))

