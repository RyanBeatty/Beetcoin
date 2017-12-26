module TribeCoin.Arbitrary where

import TribeCoin.Types 
  ( TribeCoinAddress (..), PubKey (..), PubKeyHash (..), AddressChecksum (..)
  , TxOut (..), Amount (..), TxId (..))

import Crypto.Hash (SHA256 (..), digestFromByteString, hashWith)
import Crypto.Secp256k1 (importPubKey, secKey, derivePubKey)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS (ByteString, pack, cons, take)
import Data.Either (either)
import Data.Maybe (fromJust)
import Data.Serialize (encode, decode)
import Data.Word (Word8, Word32, Word64)
import Test.Tasty.QuickCheck (Arbitrary, Gen, arbitrary, vector)

-- | Represents a random, valid public key.
newtype RandomPubKey = RandomPubKey { _unRandomPubKey :: PubKey }
  deriving (Show)

-- | A valid random public key hash.
newtype RandomPubKeyHash = RandomPubKeyHash { _unRandomPubKeyHash :: PubKeyHash }
  deriving (Show)

-- | A valid random address.
newtype RandomTribeCoinAddress = RandomTribeCoinAddress { _unRandomTribeCoinAddress :: TribeCoinAddress }
  deriving (Show)

-- | A valid random transaction output.
newtype RandomTxOut = RandomTxOut { _unRandomTxOut :: TxOut }
  deriving (Show)

-- | A valid random transaction id.
newtype RandomTxId = RandomTxId { _unRandomTxId :: TxId }
  deriving (Show)

instance Arbitrary RandomPubKey where
  -- | Generate a random public key by first generating a random private key and then deriving a public
  -- key from the private key.
  arbitrary = RandomPubKey . PubKey . derivePubKey . fromJust . secKey . BS.pack <$> vector 32

instance Arbitrary RandomPubKeyHash where
  arbitrary = do
    -- Generate a random 160 byte digest representing a ripemd160 digest.
    bytes <- vector 20 :: Gen [Word8]
    return . RandomPubKeyHash . PubKeyHash . fromJust . digestFromByteString . BS.pack $ bytes

instance Arbitrary RandomTribeCoinAddress where
  arbitrary = do
    -- Generate a random public key hash.
    pubkey_hash <- _unRandomPubKeyHash <$> arbitrary :: Gen PubKeyHash
    -- Generate the checksum by prepending the version byte (0x00) and then sha256 hashing the
    -- bytes twice. The checksum is then the first 4 bytes of the hash.
    let bytes    = (0x00 :: Word8) `BS.cons` (encode pubkey_hash)
    let checksum = AddressChecksum . either (error "Failed to parse AddressChecksum!") (id) . decode . BS.take 4 .
                   convert . hashWith SHA256 . hashWith SHA256 $ bytes
    return . RandomTribeCoinAddress $ TribeCoinAddress pubkey_hash checksum

instance Arbitrary RandomTxOut where
  -- | Generate a random transaction output by sending a random amount to a random address.
  arbitrary = do
    amount <- arbitrary :: Gen Word64
    address <- _unRandomTribeCoinAddress <$> arbitrary :: Gen TribeCoinAddress
    return . RandomTxOut $ TxOut (Amount amount) address

instance Arbitrary RandomTxId where
  -- | Generate a random transaction id by generating a random sha256 hash.
  arbitrary = RandomTxId . TxId . fromJust . digestFromByteString . BS.pack <$> vector 32
