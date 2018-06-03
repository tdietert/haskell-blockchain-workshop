{-# LANGUAGE ScopedTypeVariables #-}

module Serialization
  ( testSerialization
  ) where

import Protolude

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck.Monadic (monadicIO, run, assert)

import Data.Binary (Binary, encode, decode)

import Cryptography

testSerialization :: TestTree
testSerialization =
  testGroup "Serialization Tests"
    [ testPrivateKeySerialization
    , testSignatureSerialization
    ]

testPrivateKeySerialization :: TestTree
testPrivateKeySerialization = do
  testProperty "Test Private Key Serialization" $ monadicIO $ do
    (_, sk) <- run newKeyPair
    assert (binaryRoundTrip sk)

testSignatureSerialization :: TestTree
testSignatureSerialization = do
  testProperty "Test Signature Serialization" $
    \(randStr :: [Char]) -> monadicIO $ do
      (_, sk) <- run newKeyPair
      sig <- run $ sign sk (toS randStr)
      assert (binaryRoundTrip sig)

binaryRoundTrip :: (Eq a, Binary a) => a -> Bool
binaryRoundTrip x = x == decode (encode x)
