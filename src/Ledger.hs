{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ledger
  ( Ledger(..)
  , transfer
  , reward
  , holdings
  ) where

import Protolude

import Data.Map (Map, lookup, insertWith, adjust)

import Address (Address)

newtype Ledger = Ledger (Map Address Int)
  deriving (Show, Eq, Monoid)

data InvalidTransfer = InsufficientFunds
  { transferer :: Address, amount :: Int, balance :: Int }
  deriving (Show)

-- | Transfer an amount from one address to another. The transferer must
-- currently exist in the ledger, but the transferee does not have to.
transfer :: Ledger -> Address -> Address -> Int -> Either InvalidTransfer Ledger
transfer ledger from' to' amount' = do
  ledger' <- subtractAmount ledger from' amount'
  Right (addAmount ledger' to' amount')

-- | Add an amount of funds to a given address
reward :: Ledger -> Address -> Int -> Ledger
reward ledger to' amount' = addAmount ledger to' amount'

-- | Add an amount to an Address. The Address does not already have to exist for
-- an amount to be transferred to it.
addAmount :: Ledger -> Address -> Int -> Ledger
addAmount (Ledger ledger) to' amount' =
  Ledger (insertWith (+) to' amount' ledger)

-- | Subtract an amount from an address. This function can fail if the supplied
-- address does not have enough balance.
subtractAmount :: Ledger -> Address -> Int -> Either InvalidTransfer Ledger
subtractAmount (Ledger ledger) from' amount' =
  case lookup from' ledger of
    Nothing                  -> Left (InsufficientFunds from' amount' 0)
    Just bal | bal < amount' -> Left (InsufficientFunds from' amount' bal)
             | otherwise     ->
                 let newLedger = adjust (flip (-) amount') from' ledger
                  in Right (Ledger newLedger)

-- | Lookup the holdings of a given address
holdings :: Address -> Ledger -> Int
holdings addr (Ledger ledger) =
  case lookup addr ledger of
    Nothing  -> 0
    Just bal -> bal
