module Main (main) where

import Control.Monad
import System.IO

import Test.Tasty
import Tree (tests)

main :: IO ()
main = do
  tests >>= defaultMain

