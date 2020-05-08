{-# OPTIONS -Wno-missing-export-lists #-}

module Main where

import System.IO (IO)

import Michelson.Typed.EntryPoints.Sing.Alg.OptParse

main :: IO ()
main = parsePrintValueFromContract

