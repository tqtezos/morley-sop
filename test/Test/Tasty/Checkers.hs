{-# OPTIONS -Wno-missing-export-lists #-}

module Test.Tasty.Checkers where

import Prelude
import Test.QuickCheck.Checkers
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

testBatch :: TestBatch -> TestTree
testBatch = uncurry testGroup . fmap (fmap (uncurry testProperty))
