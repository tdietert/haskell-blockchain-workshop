{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Address
  ( Address
  , publicKeyToAddress
  , addressToPublicKey

  , parseAddress
  ) where

import Protolude

import Data.Binary (Binary, encode, decode)

import Encoding (base58, unbase58)
import Cryptography (PublicKey)

newtype Address = Address ByteString
  deriving (Show, Read, Eq, Ord, Generic, Binary)

publicKeyToAddress :: PublicKey -> Address
publicKeyToAddress = Address . base58 . toS . encode

addressToPublicKey :: Address -> Either [Char] PublicKey
addressToPublicKey (Address bs) = decode . toSL <$> unbase58 bs

parseAddress :: ByteString -> Either [Char] Address
parseAddress bs =
    const addr <$> validateAddress addr
  where
    addr = Address bs

-- | If the address can be converted back to a valid SEC_P256K1 public key then
-- the key is valid.
validateAddress :: Address -> Either [Char] ()
validateAddress addr =
  void $ addressToPublicKey addr
