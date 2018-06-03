{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cryptography
  ( -- ** Hashing
    Hash
  , parseHash
  , sha256

    -- ** ECDSA
  , PublicKey
  , PrivateKey
  , KeyPair
  , newKeyPair
  , privKeyToPubKey
  , extractPoint

  , Signature
  , sign
  , verify

    -- ** Reading / Writing to Disk
  , readPrivateKey
  , writePrivateKey

  ) where

import Prelude (show, Read(..))
import Protolude hiding (hash, show, read)

import qualified Crypto.Hash as Hash
import Crypto.Hash.Algorithms (SHA3_256)
import qualified Crypto.PubKey.ECC.ECDSA    as ECDSA
import qualified Crypto.PubKey.ECC.Generate as ECC
import qualified Crypto.PubKey.ECC.Prim     as ECC
import qualified Crypto.PubKey.ECC.Types    as ECC
import Crypto.Number.Basic (numBytes)
import Crypto.Number.Serialize (os2ip, i2ospOf_)

import qualified Data.ByteArray as BA
import qualified Data.Binary     as B
import qualified Data.ByteString as BS

import Encoding

--------------------------------------------------------------------------------
-- Hashing
--------------------------------------------------------------------------------

-- | Type & Value representing a SHA3_256 hash digest
-- Note: 'show' displays the hash encoded in base16 format
newtype Hash = Hash (Hash.Digest SHA3_256)
  deriving (Eq, Ord, Generic)

instance Show Hash where
  show (Hash d) = show d

instance Read Hash where
  readsPrec _ str =
    case parseHash (toS str) of
      Left err -> []
      Right h  -> [(h, "")]

-- | SHA3_256 hash a ByteString
sha256 :: ByteString -> Hash
sha256 = Hash . (Hash.hash :: ByteString -> Hash.Digest SHA3_256)

-- | Parse a base16 encoded SHA3_256 hash
parseHash :: ByteString -> Either [Char] Hash
parseHash b16bs =
  case unbase16 b16bs of
    Left err -> Left err
    Right bs  ->
      case Hash.digestFromByteString bs of
        Nothing -> Left "Failed to parse SHA3_256 Hash digest"
        Just d  -> Right (Hash d)

parseHash' :: ByteString -> Hash
parseHash' b16bs =
  case parseHash b16bs of
    Left err -> panic (toS err)
    Right h  -> h

--------------------------------------------------------------------------------
-- ECDSA
--------------------------------------------------------------------------------ll

-- | ECC is done using curve SECP256_K1
sec_p256k1 :: ECC.Curve
sec_p256k1 = ECC.getCurveByName ECC.SEC_p256k1

-- | Number of bytes of the curve prime
curveSize :: Int
curveSize = numBytes (ECC.ecc_p cp)
  where
    (ECC.CurveFP cp) = sec_p256k1

newtype PublicKey  = PublicKey ECDSA.PublicKey
  deriving (Show, Eq)

newtype PrivateKey = PrivateKey ECDSA.PrivateKey
  deriving (Show, Eq)

type KeyPair = (PublicKey, PrivateKey)

-- | (pk, sk) <- new
-- Returns a new elliptic curve key pair.
--
-- WARNING: Vulnerable to timing attacks.
newKeyPair :: IO KeyPair
newKeyPair = bimap PublicKey PrivateKey <$> ECC.generate sec_p256k1

-- | Create a public key from a secret key
privKeyToPubKey :: PrivateKey -> PublicKey
privKeyToPubKey (PrivateKey key) =
    PublicKey (ECDSA.PublicKey curve point)
  where
    curve  = ECDSA.private_curve key
    curve' = ECC.common_curve curve
    g      = ECC.ecc_g curve'
    point  = ECC.pointMul curve (ECDSA.private_d key) g

-- | UNSAFE: Does not validate (x,y) are valid coordinate to secp256k1
mkPublicKey :: (Integer, Integer) -> Either [Char] PublicKey
mkPublicKey (x,y)
  | ECC.isPointValid sec_p256k1 point =
      Right $ PublicKey (ECDSA.PublicKey sec_p256k1 point)
  | otherwise = Left "Invalid SEC_P256K1 public key point"
  where
    point = ECC.Point x y

extractPoint :: ECDSA.PublicKey -> (Integer, Integer)
extractPoint pubkey = (x,y)
  where
    ECC.Point x y = ECDSA.public_q pubkey

newtype Signature = Signature ECDSA.Signature
  deriving (Show, Read, Eq)

-- | SHA3_256 hashes a msg before signing with the given PrivateKey
sign :: PrivateKey -> ByteString -> IO Signature
sign (PrivateKey sk) =
  fmap Signature . ECDSA.sign sk Hash.SHA3_256

-- | Verify a signature of a SHA3_256 encoded bytestring
verify :: PublicKey -> Signature -> ByteString -> Bool
verify (PublicKey pk) (Signature sig) =
  ECDSA.verify Hash.SHA3_256 pk sig

--------------------------------------------------------------------------------
-- Reading / Writing to Disk
--------------------------------------------------------------------------------

readPrivateKey :: FilePath -> IO (Either [Char] PrivateKey)
readPrivateKey fp = do
  handle (\(e :: SomeException) -> pure . Left $ show e) $
    Right . B.decode . toSL <$> BS.readFile fp

writePrivateKey :: FilePath -> PrivateKey -> IO ()
writePrivateKey fp = BS.writeFile fp . toS . B.encode

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

instance B.Binary Hash where
  put = putHash
  get = getHash

putHash :: Hash -> B.Put
putHash (Hash d) =
  (B.put :: ByteString -> B.Put) (BA.convert d)

getHash :: B.Get Hash
getHash = do
  mDigest <- Hash.digestFromByteString <$> (B.get :: B.Get ByteString)
  case mDigest of
    Nothing -> panic "Failed to decode SHA3_256 hash from ByteString"
    Just d  -> pure (Hash d)

instance B.Binary PublicKey where
  put = putPublicKey
  get = getPublicKey

putPublicKey :: PublicKey -> B.Put
putPublicKey (PublicKey pk) = B.put pkBS
  where
    (x,y) = extractPoint pk
    pkBS = i2ospOf_ curveSize x <> i2ospOf_ curveSize y :: ByteString

getPublicKey :: B.Get PublicKey
getPublicKey = do
    ePubKey <- mkPublicKey . xAndY <$> B.get
    either (panic . toS) (pure . identity) ePubKey
  where
    xAndY = bimap os2ip os2ip . BS.splitAt curveSize

instance B.Binary PrivateKey where
  put = putPrivateKey
  get = getPrivateKey

putPrivateKey :: PrivateKey -> B.Put
putPrivateKey (PrivateKey sk) = B.put (ECDSA.private_d sk)

getPrivateKey :: B.Get PrivateKey
getPrivateKey = PrivateKey . ECDSA.PrivateKey sec_p256k1 <$> B.get

instance B.Binary Signature where
  put = putSignature
  get = getSignature

putSignature :: Signature -> B.Put
putSignature (Signature sig) = B.put sigBS
  where
    (r, s) = (ECDSA.sign_r sig, ECDSA.sign_s sig)
    sigBS = i2ospOf_ curveSize r <> i2ospOf_ curveSize s :: ByteString

getSignature :: B.Get Signature
getSignature = do
    (r, s) <- rAndS <$> B.get
    pure $ Signature (ECDSA.Signature r s)
  where
    rAndS = bimap os2ip os2ip . BS.splitAt curveSize
