{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Transaction
  ( Transaction(..)
  , TransactionHeader(..)
  , hashTransaction
  , transferTransaction
  , rewardTransaction

    -- ** Validation & Application
  , InvalidTransaction(..)
  , applyTransactions
  , applyTransaction
  ) where

import Protolude

import Data.Binary (Binary, encode)

import Address (Address, publicKeyToAddress, addressToPublicKey)
import Cryptography
  ( PrivateKey
  , Signature
  , Hash
  , privKeyToPubKey
  , sign
  , verify
  , sha256
  )
import Ledger (Ledger, transfer, reward)

data Transaction = Transaction
  { issuer    :: Address           -- ^ Issuer of the transaction
  , header    :: TransactionHeader -- ^ Transaction data
  , signature :: Signature         -- ^ Signature of the 'TransctionHeader'
  } deriving (Show, Eq, Read, Generic, Binary)

data TransactionHeader
  = Transfer { to :: Address, amount ::  Int } -- ^ Transfer an amount to another Address
  | Reward   { amount :: Int }                 -- ^ Miner's reward transaction
  deriving (Show, Read, Eq, Generic, Binary)

--------------------------------------------------------------------------------
-- Creation
--------------------------------------------------------------------------------

-- | Create a Transfer transaction to transfer some holdings from the issuer
-- address of the transaction to the address supplied as an argument
transferTransaction
  :: PrivateKey
  -> Address
  -> Int
  -> IO Transaction
transferTransaction sk toAddr amnt =
  transaction sk (Transfer toAddr amnt)

-- | Create a Reward transaction to be appended to the end of the transaction
-- list in a block that was mined by a node on the network.
rewardTransaction
  :: PrivateKey
  -> Int
  -> IO Transaction
rewardTransaction sk amnt =
  transaction sk (Reward amnt)

-- | Helper function to construct a valid transaction
transaction
  :: PrivateKey        -- ^ Private Key to sign the transaction with
  -> TransactionHeader -- ^ Transaction data to sign
  -> IO Transaction
transaction sk hdr = do
    sig <- sign sk (toSL $ encode hdr)
    pure $ Transaction orig hdr sig
  where
    orig = publicKeyToAddress (privKeyToPubKey sk)

--------------------------------------------------------------------------------
-- Hashing
--------------------------------------------------------------------------------

hashTransaction :: Transaction -> Hash
hashTransaction = sha256 . toS . encode

--------------------------------------------------------------------------------
-- Verification
--------------------------------------------------------------------------------

-- | Verify (using ECDSA.verify) that the transaction issuer has indeed signed
-- the transaction header with the private key corresponding to the issuer's
-- address.
verifyTransaction :: Transaction -> Either [Char] ()
verifyTransaction tx@(Transaction orig hdr sig) =
  case addressToPublicKey orig of
    Left err -> Left err
    Right pk
      | verify pk sig (toS $ encode hdr) -> Right ()
      | otherwise -> Left $ "Failed to verify transaction with hash: " <> txHash
  where
    txHash :: [Char]
    txHash = show $ hashTransaction tx

--------------------------------------------------------------------------------
-- Application
--------------------------------------------------------------------------------

data InvalidTransaction = InvalidTransaction Transaction [Char]
  deriving (Show)

-- | Apply a list of Transactions to the current ledger state, accumulating the
-- resulting ledger state and invalid transactions along the way.
applyTransactions :: Ledger -> NonEmpty Transaction -> (Ledger, [InvalidTransaction])
applyTransactions initLedger txs =
    foldl applyTransaction' (initLedger, []) txs
  where
    -- Apply a transaction to a given ledger state
    applyTransaction' :: (Ledger, [InvalidTransaction]) -> Transaction -> (Ledger, [InvalidTransaction])
    applyTransaction' (ledger, itxs) tx =
      case applyTransaction ledger tx of
        Left err        -> (ledger, itxs ++ [err])
        Right newLedger -> (newLedger, itxs)

-- | Apply a Transaction to the current Ledger state
applyTransaction :: Ledger -> Transaction -> Either InvalidTransaction Ledger
applyTransaction  ledger tx =
  first mkInvalidTx $ do
    verifyTransaction tx
    case header tx of
      Transfer to' amount'
        | to' == issuer tx -> Left "Cannot transfer to self"
        | amount' <= 0     -> Left "Amount transferred must be > 0"
        | otherwise        -> first show (transfer ledger (issuer tx) to' amount')
      Reward amount'       -> Right (reward ledger (issuer tx) amount')
  where
    mkInvalidTx = InvalidTransaction tx
