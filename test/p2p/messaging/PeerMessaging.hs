
module Main where

import Protolude hiding (null, toList, filter)

import Control.Distributed.Process
import Control.Distributed.Process.Node

import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.HUnit (testCaseSteps, assertBool, assertFailure)

import Data.List ((!!))
import Data.Map hiding (map)

import Block (Blockchain(..), Block(..), genesisBlockchain)
import Cryptography (Hash, newKeyPair, privKeyToPubKey)
import Ledger (holdings)

import Network.P2P (createLocalNode)
import Network.P2P.Node
  ( NodeEnv(..)
  , NodeConfig(..)
  , Peer(..)
  , initNodeEnv
  , askLedger
  , askLatestBlock
  , askTxHash
  , addPeer
  )
import Network.P2P.Message
import Network.P2P.Discovery

import System.Random (randomRIO)
import System.Timeout (timeout)

main :: IO ()
main = defaultMain testPeerMessaging

type NodeMap = Map NodeId (LocalNode, NodeEnv)

testPeerMessaging :: TestTree
testPeerMessaging = do
  testCaseSteps "Test peer messaging protocol" $ \step -> do

    step "Create 10 Nodes"
    nodeMap <- genNodeMap 10
    forM_ (map (snd . snd) $ toList nodeMap) $
      updatePeersList (keys nodeMap)

    step "Boot all localNode Messaging processes"
    forM_ nodeMap $ \(localNode, nodeEnv) ->
      forkProcess localNode (messagingProc nodeEnv)
    threadDelay 1000000

    step "Mine 1 block so that one node has holdings"
    mineBlockRandomNode nodeMap

    step "Mine 10 random blocks with 10 random transactions"
    forM_ [1..10] $ \n -> do
      step $ "  Generating 10 txs for block " <> show n
      forM_ [1..10] $ \m -> do
        step $ "    Generating random transaction " <> show m
        issueRandomTransfer nodeMap
      step $ "  Mining block " <> show n
      mineBlockRandomNode nodeMap

    step "Test joining an existing network"
    lastNode@(localNode, nodeEnv) <- genNode
    updatePeersList (keys nodeMap) nodeEnv
    forkProcess localNode $ do
      messagingProc nodeEnv

    waitAllCorrectBlock 11 (singleton (localNodeId localNode) lastNode)

  where
    genNodeMap :: Int -> IO NodeMap
    genNodeMap n = do
      nodes <- replicateM n genNode
      let nids = map (localNodeId . fst) nodes
      pure (Data.Map.fromList $ zip nids nodes)

    genNode :: IO (LocalNode, NodeEnv)
    genNode = do
      (Right localNode) <- createLocalNode "127.0.1.1" "0"
      sk <- snd <$> newKeyPair
      (Right nodeEnv) <- initNodeEnv sk genesisBlockchain
      pure (localNode, nodeEnv)

    updatePeersList :: [NodeId] -> NodeEnv -> IO ()
    updatePeersList nids nodeEnv =
      mapM_ (addPeer nodeEnv . Peer) nids

    -- Main function choosing a random node to mine a block and checking for the
    -- proper number of transactions
    mineBlockRandomNode :: NodeMap -> IO ()
    mineBlockRandomNode nodeMap = do
      (_, (localNode, nodeEnv)) <- getRandomMapElem nodeMap
      runProcess localNode $ do
        blockIdx <- mineBlock nodeEnv
        liftIO (waitAllCorrectBlock blockIdx nodeMap)

    -- Main function choosing a random node to issue a transaction
    issueRandomTransfer :: NodeMap -> IO ()
    issueRandomTransfer nodeMap = do
        -- Choose a random node that has some holdings
        (_, (localNode, nodeEnv)) <- getRandomMapElem =<< hodlers
        runProcess localNode $ do
          txHash <- issueRandomTransfer' nodeEnv
          -- Wait for all nodes to receive the transaction
          liftIO (waitAllTransaction txHash nodeMap)
      where
        nodeAddrs = map (address . nodeConfig . snd) (elems nodeMap)

        hodlers = fmap fromList $ filterM isHodler (toList nodeMap)
          where
            -- Check if the node
            isHodler (_,(_, nodeEnv)) = do
              let nodeAddr = address (nodeConfig nodeEnv)
              bal <- holdings nodeAddr <$> askLedger nodeEnv
              pure (not (bal == 0))

        issueRandomTransfer' :: NodeEnv -> Process Hash
        issueRandomTransfer' nodeEnv = do
          let nodeAddr = address (nodeConfig nodeEnv)
          bal <- liftIO (holdings nodeAddr <$> askLedger nodeEnv)
          randToAddr <- liftIO (getRandomListElem nodeAddrs)
          randAmount <- liftIO (randomRIO (1, bal))
          issueTransfer nodeEnv randToAddr randAmount


getRandomListElem :: [a] -> IO a
getRandomListElem lst = do
  randAddrIdx <- randomRIO (0, length lst - 1)
  pure (lst !! randAddrIdx)

getRandomMapElem :: Map a b -> IO (a, b)
getRandomMapElem abmap = do
  randAddrIdx <- randomRIO (0, size abmap - 1)
  pure (elemAt randAddrIdx abmap)


waitAllTransaction :: Hash -> NodeMap -> IO ()
waitAllTransaction hash nodeMap = do
    mRes <- waitAll nodeMap predicate
    case mRes of
      Nothing -> assertFailure "waitAllCorrectTransaction timed out."
      Just _  -> pure ()
  where
    predicate nodeEnv = askTxHash nodeEnv hash

waitAllCorrectBlock :: Int -> NodeMap -> IO ()
waitAllCorrectBlock n nodeMap = do
    mRes <- waitAll nodeMap predicate
    case mRes of
      Nothing -> assertFailure "waitAllCorrectBlock timed out."
      Just _  -> pure ()
  where
    predicate nodeEnv = do
      latestBlock <- askLatestBlock nodeEnv
      let idx = index latestBlock
      putText $ "Latest Block is " <> show idx <> " and should be " <> show n
      pure (n == index latestBlock)

-- | Wait for all nodes to satisfy a predicate
waitAll :: NodeMap -> (NodeEnv -> IO Bool) -> IO (Maybe ())
waitAll nodeMap predicate = timeout 5000000 (waitAll' nodeMap)
  where
    waitAll' nodeMap'
      | null nodeMap' = pure ()
      | otherwise    = do
          newNodeMap <-
            fmap fromList $
              flip filterM (toList nodeMap) $ \(_, (_, nodeEnv)) ->
                not <$> predicate nodeEnv
          threadDelay 10000
          waitAll' newNodeMap
