{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Network.P2P.Node where

import Protolude hiding (head)

import Control.Distributed.Process (NodeId, Process, nsendRemote)

import qualified Data.Binary as B
import Data.List ((\\))
import Data.List.NonEmpty (head)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Address      (Address, publicKeyToAddress)
import Block        (Blockchain(..), Block, InvalidBlock, lookupBlock, applyBlockchain, applyBlock)
import qualified Block as Block
import Cryptography (PrivateKey, privKeyToPubKey, Hash)
import Ledger       (Ledger)
import Transaction  (Transaction, InvalidTransaction(..), hashTransaction, applyTransactions)

import System.IO.Error (userError)

--------------------------------------------------------------------------------
-- Peer
--------------------------------------------------------------------------------

newtype Peer = Peer { unPeer :: NodeId }
  deriving (Eq, Ord, Generic, B.Binary, Typeable)

type Peers = Set.Set Peer

--------------------------------------------------------------------------------
-- Node
--------------------------------------------------------------------------------

data NodeConfig = NodeConfig
  { privateKey :: PrivateKey
  , address    :: Address
  }

data NodeState = NodeState
  { peers      :: MVar Peers
  , blockchain :: MVar Blockchain
  , ledger     :: MVar Ledger
  , txPool     :: MVar TxPool
  }

data NodeEnv = NodeEnv
  { nodeConfig :: NodeConfig
  , nodeState  :: NodeState
  }

-- | Initialize the node state with a blockchain. For bootstrapping a fresh node,
-- this will consist of only the genesis block.
initNodeState :: Blockchain -> IO (Either InvalidBlock NodeState)
initNodeState blockchain' = do
  case applyBlockchain mempty blockchain' of
    Left err      -> pure (Left err)
    Right ledger' -> do
      peersMVar      <- newMVar mempty
      blockchainMVar <- newMVar blockchain'
      ledgerMVar     <- newMVar ledger'
      txPoolMVar     <- newMVar initTxPool
      pure $ Right (NodeState peersMVar blockchainMVar ledgerMVar txPoolMVar)

initNodeEnv :: PrivateKey -> Blockchain -> IO (Either InvalidBlock NodeEnv)
initNodeEnv sk blockchain' = do
    eNodeState <- initNodeState blockchain'
    pure $ NodeEnv nodeConfig' <$> eNodeState
  where
    nodeConfig' = NodeConfig
      { privateKey = sk
      , address    = publicKeyToAddress (privKeyToPubKey sk)
      }

modifyNodeState
  :: NodeEnv               -- ^ Node Environment
  -> (NodeState -> MVar a) -- ^ Field to modify
  -> (a -> IO a)           -- ^ Modify function
  -> IO ()
modifyNodeState nodeEnv field f = do
  let mvar = field (nodeState nodeEnv)
  modifyMVar_ mvar f

-- Read the state of a node MVar given a node environment
readNodeState
  :: NodeEnv
  -> (NodeState -> MVar a) -- ^ Field to read
  -> IO a
readNodeState nodeEnv f = do
  let mvar = f (nodeState nodeEnv)
  liftIO (readMVar mvar)

--------------------------------------------------------------------------------
-- Query Node State
--------------------------------------------------------------------------------

askPeers :: NodeEnv -> IO Peers
askPeers nodeEnv = readNodeState nodeEnv peers

askLedger :: NodeEnv -> IO Ledger
askLedger nodeEnv = readNodeState nodeEnv ledger

askBlock  :: NodeEnv -> Int -> IO (Either [Char] Block)
askBlock nodeEnv idx = do
  chain <- readNodeState nodeEnv blockchain
  pure $ case lookupBlock idx chain of
    Nothing    -> Left $ "No block with index " ++ show idx
    Just block -> Right block

askLatestBlock :: NodeEnv -> IO Block
askLatestBlock nodeEnv =
  head . unBlockchain <$> readNodeState nodeEnv blockchain

-- | Get the current state of the transaction pool
askTxPool :: NodeEnv -> IO TxPool
askTxPool nodeEnv = readNodeState nodeEnv txPool

-- | Lookup a transaction in the transaction pool by transaction hash
askTxHash :: NodeEnv -> Hash -> IO Bool
askTxHash nodeEnv h = do
  txHashMap <- txPoolHashMap <$> askTxPool nodeEnv
  case Map.lookup h txHashMap of
    Nothing -> pure False
    Just _  -> pure True

--------------------------------------------------------------------------------
-- Update Node State
--------------------------------------------------------------------------------

addPeer :: NodeEnv -> Peer -> IO ()
addPeer nodeEnv peer =
  modifyNodeState nodeEnv peers $ pure . Set.insert peer

removePeer :: NodeEnv -> Peer -> IO ()
removePeer nodeEnv peer =
  modifyNodeState nodeEnv peers $ pure . Set.delete peer

addBlock :: NodeEnv -> Block -> IO (Either InvalidBlock ())
addBlock nodeEnv newBlock = do
  latestBlock <- askLatestBlock nodeEnv
  currLedger  <- askLedger nodeEnv
  case applyBlock currLedger latestBlock newBlock of
    Left err        -> pure (Left err)
    Right newLedger -> Right <$> do
      modifyNodeState nodeEnv ledger (pure . const newLedger)
      modifyNodeState nodeEnv blockchain (pure . Block.addBlock newBlock)

-- | Add a transaction to the Transaction Pool if it doesn't already exist
addTxPool :: NodeEnv -> Transaction -> IO (Either [Char] ())
addTxPool nodeEnv tx = do
  currTxPool <- readNodeState nodeEnv txPool
  case addTransaction currTxPool tx of
    Left err -> pure (Left err)
    Right newTxPool -> do
      modifyNodeState nodeEnv txPool (pure . const newTxPool)
      pure (Right ())

-- | Return all the valid transactions in the transaction pool and remove the
-- invalid ones.
pruneTxPool :: NodeEnv -> IO ([Transaction],[InvalidTransaction])
pruneTxPool nodeEnv = do
  initTxs <- getOrderedTxs <$> askTxPool nodeEnv
  case nonEmpty initTxs of
    Nothing -> pure ([], [])
    Just nonEmptyTxs -> do
      ledger <- askLedger nodeEnv
      let invalidTxs = snd (applyTransactions ledger nonEmptyTxs)
      liftIO (removeTxPoolManyInvalid nodeEnv invalidTxs)
      let validTxs = initTxs \\ (map unInvalidTx invalidTxs)
      pure (validTxs, invalidTxs)
  where
    unInvalidTx (InvalidTransaction tx _) = tx


removeTxPoolMany :: NodeEnv -> [Hash] -> IO ()
removeTxPoolMany nodeEnv hashes =
  forM_ hashes $ \h -> do
    eRes <- removeTxPool nodeEnv h
    case eRes of
      Left err -> throwIO (userError err)
      Right _  -> pure ()

removeTxPoolManyInvalid :: NodeEnv -> [InvalidTransaction] -> IO ()
removeTxPoolManyInvalid nodeEnv invalidTxs = do
  let invalidTxHashes =
        flip map invalidTxs $ \(InvalidTransaction tx _) ->
          hashTransaction tx
  removeTxPoolMany nodeEnv invalidTxHashes

-- | Remove a transaction from the Transaction Pool if it exists
removeTxPool :: NodeEnv -> Hash -> IO (Either [Char] ())
removeTxPool nodeEnv txHash = do
  currTxPool <- readNodeState nodeEnv txPool
  case removeTransaction currTxPool txHash of
    Left err -> pure (Left err)
    Right newTxPool -> do
      modifyNodeState nodeEnv txPool (pure . const newTxPool)
      pure (Right ())

--------------------------------------------------------------------------------
-- Transaction Pool
--------------------------------------------------------------------------------

-- | Datastructure containing the transactions received by the node.
-- For efficiency, transactions are indexed by their hash, and the number of the
data TxPool = TxPool
  { txPoolHashMap  :: Map Hash (Int, Transaction)
  , txPoolSize :: Int
  }

initTxPool :: TxPool
initTxPool = TxPool mempty 0

addTransaction :: TxPool -> Transaction -> (Either [Char] TxPool)
addTransaction txPool' tx =
  case Map.lookup txHash (txPoolHashMap txPool') of
    Nothing -> Right (insertTransaction txPool' tx)
    Just _  -> Left $ "Cannot insert duplicate transaction: " ++ show txHash
  where
    txHash = hashTransaction tx

removeTransaction :: TxPool -> Hash -> (Either [Char] TxPool)
removeTransaction txPool' txHash =
  case Map.lookup txHash (txPoolHashMap txPool') of
    Nothing -> Left $ "Cannot remove a non-existant transasction: " ++ show txHash
    Just _  -> Right $ deleteTransaction txPool' txHash

-- | Insert a transaction into the TxPool.
-- Warning: Unsafe. Does not check for duplicate transactions, therefore if the
-- transaction already exists in the mempool, the size will be update incorrectly.
insertTransaction :: TxPool -> Transaction -> TxPool
insertTransaction (TxPool hashMap size) tx =
  let newSize = size + 1
   in TxPool
        { txPoolHashMap = Map.insert (hashTransaction tx) (newSize, tx) hashMap
        , txPoolSize = newSize
        }

-- | Delete a transaction from the TransactionPool
-- Warning: Unsafe. Does not check that the transaction exists, therefore the
-- size may not be updated properly if the transaction does not exist in the TxPool
deleteTransaction :: TxPool -> Hash -> TxPool
deleteTransaction (TxPool hashMap size) txHash = TxPool
  { txPoolHashMap = Map.delete txHash hashMap
  , txPoolSize = size - 1
  }

getOrderedTxs :: TxPool -> [Transaction]
getOrderedTxs =
  map snd . sortBy (comparing fst) . Map.elems . txPoolHashMap
