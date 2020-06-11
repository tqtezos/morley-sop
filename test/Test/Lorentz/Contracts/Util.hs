{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS -Wno-missing-export-lists #-}

module Test.Lorentz.Contracts.Util where
import Lorentz.Contracts.Util ()

import Lorentz.Value

import Data.Function

import qualified Tezos.Crypto.Ed25519 as Ed25519
import qualified Tezos.Crypto.Secp256k1 as Secp256k1
import qualified Tezos.Crypto.P256 as P256

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes

test_Ord_Ed25519_PublicKey :: TestTree
test_Ord_Ed25519_PublicKey = testGroup "Ed25519.PublicKey" [testBatch $ ord (const $ arbitrary @Ed25519.PublicKey)]

test_Ord_Secp256k1_PublicKey :: TestTree
test_Ord_Secp256k1_PublicKey = testGroup "Secp256k1.PublicKey" [testBatch $ ord (const $ arbitrary @Secp256k1.PublicKey)]

test_Ord_P256_PublicKey :: TestTree
test_Ord_P256_PublicKey = testGroup "P256.PublicKey" [testBatch $ ord (const $ arbitrary @P256.PublicKey)]

test_Ord_PublicKey :: TestTree
test_Ord_PublicKey = testGroup "PublicKey" [testBatch $ ord (const $ arbitrary @PublicKey)]


