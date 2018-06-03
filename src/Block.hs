{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Block
  ( Block(..)
  , hashBlock
  , computeMerkleRoot
  , genesisBlockchain
  , genesisBlock
  , mineBlock

  , Blockchain(..)
  , lookupBlock
  , addBlock

  , InvalidBlock
  , applyBlockchain
  , applyBlock

  -- For testing
  , validateMerkleRoot
  ) where

import Protolude hiding (reverse)

import Crypto.Hash.MerkleTree (MerkleRoot(..), emptyHash, mkMerkleTree, mtRoot, getMerkleRoot)

import Data.Binary (Binary, encode)
import qualified Data.ByteString as BS
import Data.List (findIndex)
import Data.List.NonEmpty ((<|), reverse)

import Cryptography (PrivateKey, Hash, sha256, parseHash)
import Ledger (Ledger)
import Transaction (Transaction, TransactionHeader(..), InvalidTransaction, rewardTransaction, applyTransactions)
import qualified Transaction as T

import GHC.IO.Unsafe (unsafePerformIO)

data Block = Block
  { index        :: Int
  , header       :: BlockHeader
  , transactions :: [Transaction]
  } deriving (Show, Read, Generic, Binary)

data BlockHeader = BlockHeader
  { prevBlockHash :: Hash -- ^ Hash of the previous block
  , merkleRoot    :: Hash -- ^ Root of the merkle tree of all block transactions
  , nonce         :: Int  -- ^ Value to be incremented for PoW, starts at 0
  } deriving (Show, Read, Generic, Binary)

-- | Hash a block for the next block's previous block hash when creating a new block
hashBlock :: Block -> Hash
hashBlock = hashBlockHeader . header

-- | Hash a block header, serializing it first
hashBlockHeader :: BlockHeader -> Hash
hashBlockHeader = sha256 . toS . encode

-- Create the merkle root hash of the list of block transactions
computeMerkleRoot :: [Transaction] -> Hash
computeMerkleRoot blockTxs =
  let txBSs         = map (toS . encode) blockTxs
      merkleRootTxs = mtRoot (mkMerkleTree txBSs)
  in case parseHash (getMerkleRoot merkleRootTxs) of
       Left _               -> panic "Failed to parse MerkleRoot as Base16 encoded hash"
       Right merkleRootHash -> merkleRootHash

-- | Datastructure storing a chain of blocks from most recent to genesis block
-- Block(N) : Block(N-1) : ... : Block(0)
newtype Blockchain = Blockchain
  { unBlockchain :: NonEmpty Block }

-- | Lookup a block in the blockchain by index
lookupBlock :: Int -> Blockchain -> Maybe Block
lookupBlock n (Blockchain chain) =
  find ((==) n . index) (toList chain)

-- | Add a block to the blockchain
-- Warning: Does not validate the block
addBlock :: Block -> Blockchain -> Blockchain
addBlock b (Blockchain chain) = Blockchain (b <| chain)

--------------------------------------------------------------------------------
-- Creation
--------------------------------------------------------------------------------

genesisBlockchain :: Blockchain
genesisBlockchain = Blockchain (genesisBlock :| [])

-- | The initial block in the chain. This block must be the same on all nodes
-- wishing to transact on the same blockchain.
genesisBlock :: Block
genesisBlock =
    Block { index        = 0
          , header       = genesisHeader
          , transactions = []
          }
  where
    genesisHeader = BlockHeader
      { prevBlockHash = genesisPrevBlockHash
      , merkleRoot    = merkleRootHash
      , nonce         = 0
      }

    (Right genesisPrevBlockHash) =
      parseHash (toS (replicate 64 '0') :: ByteString)

    (Right merkleRootHash) =
      parseHash (getMerkleRoot emptyHash)

-- | Mine a block "on-top-of" the block supplied as an argument.
mineBlock
  :: PrivateKey    -- ^ PrivateKey to sign the reward tx
  -> Block         -- ^ Previous block (to include the previous block hash)
  -> [Transaction] -- ^ Potentially empty list of transactions
  -> IO Block
mineBlock sk prevBlock txs = do
    -- Generate and prepend the miner's "reward" transaction
    rewardTx <- rewardTransaction sk (calcReward blockIndex)
    let blockTxs    = rewardTx : txs
        merkleRoot' = computeMerkleRoot (toList blockTxs)
    pure $ Block
      { index        = blockIndex
      , header       = mkValidBlockHeader merkleRoot'
      , transactions = blockTxs
      }
  where
    -- Index of current block being mined
    blockIndex = index prevBlock + 1

    -- Number of 0's needed in the block header hash for PoW
    difficulty = calcDifficulty blockIndex

    -- Hash of the previous block, used when constructing the new header
    prevBlockHash' = hashBlock prevBlock

    -- Create a valid block header satisfying the proof of work criteria
    mkValidBlockHeader :: Hash -> BlockHeader
    mkValidBlockHeader merkleRoot' =
        proofOfWork $ BlockHeader
          { prevBlockHash = prevBlockHash'
          , merkleRoot    = merkleRoot'
          , nonce         = 0
          }
      where
        -- Calculate the nonce by repeatedly hashing the block header and
        -- incrementing the nonce value until a hash prefixed by the number of
        -- zeroes determined by the difficulty is found.
        --
        -- Note: This is *the* Proof-of-Work function
        --
        -- To test your implementation run:
        -- $ stack test hs-blockchain-workshop:hs-blockchain-test
        proofOfWork :: BlockHeader -> BlockHeader
        proofOfWork blockHdr = blockHdr

-- | The "difficulty" of the current block is defined by:
--     difficulty(block) = round(ln(index(block)))
-- i.e. The number of zeroes that must prefix a valid block's header hash is the
-- logarithm (base 2) of its index. The average nonce generated for each
-- difficulty is proportional to 16^difficulty; e.g. when generating the 16th
-- block, the difficulty will be 4, and the average nonce computed to mine a
-- block will be 16^4. When mining the 32nd block,
calcDifficulty :: Int -> Int
calcDifficulty = round . logBase (2 :: Float) . fromIntegral

-- | reward = 100 * (block difficulty + 1)
-- e.g: calcReward(block 1)  = 100
--      calcReward(block 2)  = 200
--      calcReward(block 4)  = 300
--      calcReward(block 8)  = 400
--      ...
--      calcReward(block 16) = 500
--      ...
--      calcReward(block 32) = 600
calcReward :: Int -> Int
calcReward = (*) 100 . (+1) . calcDifficulty

--------------------------------------------------------------------------------
--  Validation / Application
--------------------------------------------------------------------------------

data InvalidBlock
  = InvalidBlockIndex       { expectedIdx :: Int, actualIdx :: Int }
  | InvalidProofOfWork      { expectedPref :: Int, actualPref :: Int }
  | InvalidPrevBlockHash    { expectedHash :: Hash, actualHash :: Hash }
  | InvalidMerkleRoot       { expectedMerkleRoot :: Hash, actualMerkleRoot :: Hash }
  | InvalidTransactionList  InvalidTransactionList
  deriving (Show)

data InvalidTransactionList
  = InvalidRewardAmount     { expectedReward  :: Int, actualReward :: Int }
  | InvalidRewardIndex      { indexOfReward   :: Maybe Int }
  | InvalidNumberRewards    { numberOfRewards :: Int }
  | InvalidTransactions     { itxs :: [InvalidTransaction] }
  | NoTransactions
  deriving (Show)

-- | Given a blockchain, produce a ledger state from repeatedly applying the
-- blocks in order, beginning at the genesisBlock.
-- Note: This function will only be used when loading a ledger state from disk
applyBlockchain :: Ledger -> Blockchain -> Either InvalidBlock Ledger
applyBlockchain ledger (Blockchain chain) =
    case reverse chain of
      genesisBlk :| []          -> applyBlockNoPrev ledger genesisBlk
      genesisBlk :| restOfChain ->
        fst <$> foldlM applyBlock_ (ledger, genesisBlk) restOfChain
  where
    applyBlock_ :: (Ledger, Block) -> Block -> Either InvalidBlock (Ledger, Block)
    applyBlock_ (ledger_, block_) currBlock = do
      newLedger <- applyBlock ledger_ block_ currBlock
      Right (newLedger, currBlock)

-- | Apply a block to the supplied ledger state, validating each block
-- validation criteria along the way.
--
-- Blocks are valid with respect to the previous block iff:
--   - The block index supplied is one more than the current latest block index
--   - The previous block hash in the new block matches the expected previous block hash
--   - The last transaction in the block is a correctly constructed Reward transaction
--   - All the validation criteria checked in 'applyBlockNoPrev' is met
applyBlock :: Ledger -> Block -> Block -> Either InvalidBlock Ledger
applyBlock ledger prevBlock block = do
    validateBlockIndex
    validatePrevBlockHash

   -- TODO: Exercise for the workshop
    validateMerkleRoot (merkleRoot (header block)) (transactions block)

    applyBlockNoPrev ledger block
  where
    validateBlockIndex
      | expectedIndex == actualIndex = Right ()
      | otherwise = Left (InvalidBlockIndex expectedIndex actualIndex)
      where
        expectedIndex = index prevBlock + 1
        actualIndex   = index block

    validatePrevBlockHash
      | expectedPrevHash == actualPrevHash = Right ()
      | otherwise = Left (InvalidPrevBlockHash expectedPrevHash actualPrevHash)
      where
        expectedPrevHash = hashBlock prevBlock
        actualPrevHash   = prevBlockHash (header block)

-- TODO: Validate that the merkle root proposed by the block header matches
-- the computed merkle root of the transaction list in the block:
validateMerkleRoot
  :: Hash                   -- ^ Merkle root of the block being "accepted"
  -> [Transaction]          -- ^ List of transactions of the block being "accepted"
  -> Either InvalidBlock () -- ^ Returns 'InvalidMerkleRoot' if the given merkle root doesn't match the computed one
validateMerkleRoot mroot txs
  | (mroot :: Hash) == computedMerkleRoot = Right ()
  | otherwise = Left (InvalidMerkleRoot mroot computedMerkleRoot)
  where
    computedMerkleRoot = (computeMerkleRoot txs) :: Hash

-- | Apply a block to the ledger state, validating all the valid block criteria
-- that doesn't need access to the previous block:
--   - The block header hash has the proper number of zeroes as a prefix
--   - All transactions are valid (applyBlock')
applyBlockNoPrev :: Ledger -> Block -> Either InvalidBlock Ledger
applyBlockNoPrev ledger block = do
    validateProofOfWork
    validateTransactions
  where
    -- | Validate that the block header hash has a prefix of the number of zeroes
    -- determined by the currently difficulty
    validateProofOfWork
      | length actualPrefix >= difficulty  = Right ()
      | otherwise = Left (InvalidProofOfWork difficulty (length actualPrefix))
      where
        difficulty = calcDifficulty $ index block
        actualPrefix = takeWhile (== '0') (show (hashBlock block))

    -- | Validate the order and contents of the transaction list:
    -- 1) The transaction list must not be empty
    -- 2) The first transaction must be a Reward transaction
    -- 3) The remaining transactions must *only* be transfer transactions
    -- 4) The non-empty ordered list of transactions must be accumulatively valid
    validateTransactions :: Either InvalidBlock Ledger
    validateTransactions = do
      first InvalidTransactionList $
        case nonEmpty (transactions block) of
          -- The genesis block should have no transactions
          Nothing | index block == 0 -> Right ledger
          -- But all other blocks should have at least one transaction
                  | otherwise        -> Left NoTransactions
          Just blockTxs@(firstTx :| remTxs)   ->
            case T.header firstTx of
              Transfer _ _                   -> Left (InvalidRewardIndex (findRewardIndex (toList blockTxs)))
              Reward amount'
                | amount' /= expectedReward' -> Left (InvalidRewardAmount expectedReward' amount')
                | otherwise                  -> do
                    -- Search for any other reward transactions and fail if found
                    case filter isRewardTx remTxs of
                      []   -> case applyTransactions ledger blockTxs of
                        (newLedger, []) -> Right newLedger
                        (_, itxs')      -> Left (InvalidTransactions itxs')
                      rwds -> Left (InvalidNumberRewards (length rwds + 1))

      where
        expectedReward'          = calcReward (index block)

        findRewardIndex          = findIndex isRewardTx

        isRewardTx                    = isRewardHeader . T.header
        isRewardHeader (Reward _)     = True
        isRewardHeader (Transfer _ _) = False
