
module Main where

import Protolude hiding (toList)

import Control.Distributed.Process (NodeId)
import qualified Control.Distributed.Process.Node as DP

import Data.List (nub)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import Data.Map (Map, insert, delete, keys, toList, fromList)

import Block (Blockchain(..), genesisBlockchain)
import Cryptography (newKeyPair)

import Network.P2P
import Network.P2P.Node
import Network.P2P.Discovery

import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.HUnit (testCaseSteps, assertEqual, assertBool)

import System.Timeout (timeout)

testPeerDiscovery :: TestTree
testPeerDiscovery = do
  testCaseSteps "Test peer discovery protocol" $ \step -> do

    step "Initialize bootnodes"
    let numBootnodes = 1
    bootNodeMap <- initNodesAndPeerDiscovery [] numBootnodes
    let bootNodeIds = keys bootNodeMap

    step "Initialize peer nodes"
    let numPeers = 9 -- XXX generate with quickcheck
    peerNodeMap <- initNodesAndPeerDiscovery (keys bootNodeMap) numPeers

    let total = numBootnodes + numPeers
    step "Wait until bootnode discovers all peers"
    assertBool "Bootnodes failed to discover all peers" =<<
      waitAllDiscover total bootNodeMap
    step "Wait until all peers nodes discover all peers"
    assertBool "Some peers failed to find all other peers" =<<
      waitAllDiscover total peerNodeMap

  where
    initNodesAndPeerDiscovery
      :: [NodeId]
      -> Int
      -> IO (Map NodeId (MVar Peers))
    initNodesAndPeerDiscovery bootNodeIds n = do
      let accum = accumNewNodes bootNodeIds
      foldM (const . accum) mempty [1..n]

    accumNewNodes
      :: [NodeId]
      -> Map NodeId (MVar Peers)
      -> IO (Map NodeId (MVar Peers))
    accumNewNodes bootNodeIds peersMap = do
      (nid,  peersMVar) <- initPeerDiscovery bootNodeIds
      pure (insert nid peersMVar peersMap)

    initPeerDiscovery :: [NodeId] -> IO (NodeId, MVar Peers)
    initPeerDiscovery bootnodes = do
      Right node <- createLocalNode "127.0.0.1" "0"
      sk <- snd <$> newKeyPair
      (Right nodeEnv) <- initNodeEnv sk genesisBlockchain
      DP.forkProcess node (peerDiscoveryProc nodeEnv bootnodes)
      pure (DP.localNodeId node, peers (nodeState nodeEnv))

    waitAllDiscover
      :: Int
      -> Map NodeId (MVar Peers)
      -> IO Bool
    waitAllDiscover total peersMap = do
        mRes <- timeout 5000000 waitAllDiscover'
        case mRes of
          Nothing -> panic "waitAllDiscover timed out."
          Just _  -> pure True
      where
        waitAllDiscover'
          | null peersMap = pure True
          | otherwise = do
              putText "Checking if all peers are discovered by all nodes:"
              newPeersMap <- fromList <$>
                filterM checkSuccess (toList peersMap)
              threadDelay 1000000
              waitAllDiscover total newPeersMap
          where
            checkSuccess (nid, peersMVar) = do
              peers <- readMVar peersMVar
              let lenPeers = length peers
              putText $ T.intercalate " "
                [ "   ", show nid
                , "has discovered", show lenPeers
                , " peers..."
                ]
              if lenPeers == total
                 then pure False
                 else pure True

main :: IO ()
main = defaultMain testPeerDiscovery
