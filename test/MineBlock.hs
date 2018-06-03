{-# LANGUAGE ScopedTypeVariables #-}

module MineBlock
  ( testValidateMerkleRoot
  , testBlockMining
  ) where

import Protolude hiding ((!), filter)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps, assertFailure)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.QuickCheck.Gen (generate, choose)

import Data.Map (Map, (!), fromList, size, elemAt, keys, filter, delete)

import Address
import Block
import Cryptography
import Ledger
import Transaction

import System.Random (randomRIO)

testValidateMerkleRoot :: TestTree
testValidateMerkleRoot =
  testGroup "Test 'validateMerkleRoot' function"
    [ testProperty "Valid merkle root hashes are accepted" $ monadicIO $ do
        nTxs :: Int <- run (generate (choose (0,10)))
        keyPairs <- run $ replicateM 5 newKeyPair
        let keysMap = fromList (map (first publicKeyToAddress) keyPairs)
        let ledger = foldl (\l a -> reward l a 1000) mempty (keys keysMap)
        (newLedger, txs) <- run $
          foldM (\accum -> genRandomTransfer keysMap . const accum)
                (ledger, [])
                (if nTxs == 0 then [] else [0.. (nTxs - 1)])
        let merkleRoot' = computeMerkleRoot txs
        case validateMerkleRoot merkleRoot' txs of
          Left err -> assert False
          Right _  -> assert True

    , testProperty "Invalid Merkle root hashes are rejected" $ monadicIO $ do
        nTxs :: Int <- run (generate (choose (0,10)))
        keyPairs <- run $ replicateM 5 newKeyPair
        let keysMap = fromList (map (first publicKeyToAddress) keyPairs)
        let ledger = foldl (\l a -> reward l a 1000) mempty (keys keysMap)
        (newLedger, txs) <- run $
          foldM (\accum -> genRandomTransfer keysMap . const accum)
                (ledger, [])
                (if nTxs == 0 then [] else [0.. (nTxs - 1)])
        let merkleRoot' = computeMerkleRoot txs
        (_,rogueTx) <- run $ genRandomTransfer keysMap (newLedger, txs)
        let falseTxs = txs ++ rogueTx
        case validateMerkleRoot merkleRoot' falseTxs of
          Left err -> assert True
          Right _  -> assert False
    ]

testBlockMining :: TestTree
testBlockMining =
  testCaseSteps "Test Mining 15 blocks" $ \step -> do

    step "Create keys to issue transfers with"
    keyPairs <- replicateM 10 newKeyPair
    let keysMap = fromList (map (first publicKeyToAddress) keyPairs)

    step "Mine 15 Random blocks..."
    prevBlockAndLedgerMVar <- newMVar (genesisBlock, mempty)
    forM_ [1..15] $ \n -> do
      step $ "Generate random transactions and mining block " ++ show n
      (prevBlock, ledger) <- takeMVar prevBlockAndLedgerMVar
      eRes <- genRandomBlock keysMap ledger prevBlock
      case eRes of
        Left err -> assertFailure err
        Right newBlockAndLedger ->
          putMVar prevBlockAndLedgerMVar newBlockAndLedger

-- | Generate a random block given the current ledger state and the previous block
genRandomBlock
  :: Map Address PrivateKey
  -> Ledger
  -> Block
  -> IO (Either [Char] (Block, Ledger))
genRandomBlock keysMap ledger prevBlock = do
    randNumTxs <- randomRIO (10, 100) :: IO Int
    (newLedger, txs) <-
      foldM (\accum -> genRandomTransfer keysMap . const accum)
            (ledger, [])
            (if ledger == mempty then [] else [1.. randNumTxs])
    randMinerKey <- snd <$> getRandomMapElem keysMap
    newBlock <- mineBlock randMinerKey prevBlock txs
    case applyBlock ledger prevBlock newBlock of
      Left err         -> pure $ Left (show err)
      Right newLedger' -> do
        let (Just firstTx) = head (transactions newBlock)
        let (Right newLedger'') = applyTransaction newLedger firstTx
        -- Check and see if ledger calculuated by applying transactions in order
        -- during generation matches ledger calculated by applying the block
        pure $ if newLedger' == newLedger''
           then Right (newBlock, newLedger')
           else Left "Accumulated ledger states don't match!"

genRandomTransfer
  :: Map Address PrivateKey
  -> (Ledger,[Transaction])
  -> IO (Ledger,[Transaction])
genRandomTransfer keysMap (ledger', txs) = do
    tx <- genRandomTransfer' ledger'
    let (Right newLedger) = applyTransaction ledger' tx
    pure (newLedger, txs ++ [tx])
  where
    -- Generate a random transfer transaction given a non-empty Ledger state
    genRandomTransfer' :: Ledger -> IO Transaction
    genRandomTransfer' (Ledger ledger) = do
        -- Choose random transfer from holders
        (transferer, hodlings) <- getRandomMapElem hodlers
        -- Choose random address to transfer to, non including transferer address
        (randToAddr,_) <- getRandomMapElem (delete transferer keysMap)
        let transfererKey  = keysMap ! transferer
        -- Choose random amount to transfer given transferers holdings
        randAmount <- randomRIO (1, hodlings)
        transferTransaction transfererKey randToAddr randAmount
      where
        hodlers = filter (> 0) ledger

getRandomMapElem :: Map Address a -> IO (Address, a)
getRandomMapElem addrMap = do
  randAddrIdx <- randomRIO (0, size addrMap - 1)
  pure (elemAt randAddrIdx addrMap)
