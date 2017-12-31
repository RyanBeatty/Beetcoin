module TribeCoin.Arbitrary where

import TribeCoin.Types

import Crypto.Hash (SHA256 (..), digestFromByteString, hashWith)
import Crypto.PubKey.ECC.DH (calculatePublic)
import Crypto.PubKey.ECC.ECDSA (PublicKey (..), PrivateKey (..))
import Data.ByteArray (convert)
import qualified Data.ByteString as BS (ByteString, pack, cons, take)
import Data.Either (either)
import Data.Maybe (fromJust)
import Data.Serialize (encode, decode)
import Data.Word (Word8, Word32, Word64)
import Test.Tasty.QuickCheck (Arbitrary, Gen, arbitrary, vector)

newtype RandomPrivKey = RandomPrivKey { _unRandomPrivKey :: PrivKey }
  deriving (Show)

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

newtype RandomOutpoint = RandomOutpoint { _unRandomOutpoint :: Outpoint }
  deriving (Show)


instance Arbitrary RandomPrivKey where
  -- | Generate a random private key by creating a random 32 byte number.
  -- TODO: Create MonadRandom instance for Arbitrary and use the cryptonite generator functions.
  arbitrary = RandomPrivKey . either (error "Failed to parse generated PrivKey!") (id) . decode . BS.pack <$> vector 32

instance Arbitrary RandomPubKey where
  -- | Generate a random public key by first generating a random private key and then deriving a public
  -- key from the private key.
  arbitrary = RandomPubKey . mkPubKey . calculatePublic secp256k1 . private_d . _unPrivKey . _unRandomPrivKey <$> arbitrary

instance Arbitrary RandomPubKeyHash where
  arbitrary = do
    -- Generate a random 160 byte digest representing a ripemd160 digest.
    bytes <- vector 20 :: Gen [Word8]
    return . RandomPubKeyHash . PubKeyHash . fromJust . digestFromByteString . BS.pack $ bytes

instance Arbitrary RandomTribeCoinAddress where
  -- Generate a random address by generating a random public key hash.
  arbitrary = RandomTribeCoinAddress . TribeCoinAddress . _unRandomPubKeyHash <$> arbitrary

instance Arbitrary RandomTxOut where
  -- | Generate a random transaction output by sending a random amount to a random address.
  arbitrary = do
    amount <- arbitrary :: Gen Word64
    address <- _unRandomTribeCoinAddress <$> arbitrary :: Gen TribeCoinAddress
    return . RandomTxOut $ TxOut (Amount amount) address

instance Arbitrary RandomTxId where
  -- | Generate a random transaction id by generating a random sha256 hash.
  arbitrary = RandomTxId . TxId . fromJust . digestFromByteString . BS.pack <$> vector 32

instance Arbitrary RandomOutpoint where
  arbitrary = do
    index <- TxIndex <$> arbitrary
    txId  <- _unRandomTxId <$> arbitrary
    return . RandomOutpoint $ Outpoint txId index
