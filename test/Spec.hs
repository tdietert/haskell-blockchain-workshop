
module Spec where

import Protolude

import Test.Tasty (testGroup, defaultMain)

import MineBlock (testBlockMining, testValidateMerkleRoot)
import Serialization (testSerialization)

import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  defaultMain $
    testGroup "Project Tests"
      [ testSerialization
      , testValidateMerkleRoot
      , testBlockMining
      ]
