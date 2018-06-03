

module Encoding
  ( base16
  , unbase16
  , base58
  , unbase58
  , base64
  , unbase64
  ) where

import Protolude

import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString.Base58 as B58

base16 :: ByteString -> ByteString
base16 = BA.convertToBase BA.Base16

unbase16 :: ByteString -> Either [Char] ByteString
unbase16 = BA.convertFromBase BA.Base16

base58 :: ByteString -> ByteString
base58 = B58.encodeBase58 B58.bitcoinAlphabet

unbase58 :: ByteString -> Either [Char] ByteString
unbase58 bs =
  case B58.decodeBase58 B58.bitcoinAlphabet bs of
    Nothing -> Left "Bytestring is not in valid Base 58 format."
    Just rawbs -> Right rawbs

base64 :: ByteString -> ByteString
base64 = BA.convertToBase BA.Base16

unbase64 :: ByteString -> Either [Char] ByteString
unbase64 = BA.convertFromBase BA.Base16
