module Main (main) where

import Prelude
import Test.Tasty
import Tree (tests)

main :: IO ()
main = do
  tests >>= defaultMain

