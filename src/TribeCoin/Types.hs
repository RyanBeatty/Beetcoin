{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module TribeCoin.Types
    ( BlockHash (..)
    , Timestamp (..)
    , TimestampDiff (..)
    , Target (..)
    , Nonce (..)
    , BlockHeader (..)
    , Block (..)
    , Amount (..)
    , TXVersion (..)
    , PubKeyHash (..)
    , TribeCoinAddress (..)
    , ChainState (..)
    , ChainT (..)
    , TxMap (..)
    , Transaction (..)
    , SigScript (..)
    , SigMsg (..)
    , Sig (..)
    , PrivKey (..)
    , mkPrivKey
    , privKeyLowerBound
    , privKeyUpperBound
    , PubKey (..)
    , mkPubKey
    , TxOut (..)
    , TxId (..)
    , TxIndex (..)
    , Outpoint (..)
    , secp256k1
    ) where

import Control.Monad.Fail as MF (fail)
import Control.Monad.State (StateT (..))
import Control.Monad.State.Class (MonadState)
import Crypto.Hash (Digest, SHA256 (..), RIPEMD160, HashAlgorithm, digestFromByteString, hashWith)
import qualified Crypto.PubKey.ECC.ECDSA as ECC
  ( PublicKey (..), PublicPoint (..), Signature (..), PrivateKey (..), PrivateNumber)
import qualified Crypto.PubKey.ECC.Types as CPET (Point (..), Curve, CurveName (..), getCurveByName)
import qualified Crypto.Number.Serialize as CNS (i2osp, os2ip)
import Data.ByteArray (ByteArrayAccess, convert)
import qualified Data.ByteString as BS (ByteString, append, length, take, cons)
import Data.ByteString.Base58 (bitcoinAlphabet, encodeBase58, decodeBase58)
import qualified Data.HashMap.Strict as HM (HashMap)
import Data.Serialize
  ( Serialize, Get, Putter, Put, put, get, encode, decode
  , runPut, runGet, putByteString, remaining, getByteString
  , putWord8, getWord8
  )
import Data.Time (NominalDiffTime (..))
import Data.Time.Clock.POSIX (POSIXTime)
import Data.UnixTime (UnixTime (..))
import Data.Word (Word32, Word64, Word8)
import Foreign.C.Types (CTime (..))
import GHC.Generics (Generic)
import Numeric (showHex)

-- ^ The sha256 hash of a block header.
newtype BlockHash = BlockHash { _unBlockHash :: (Digest SHA256) }
      deriving (Show, Eq)

-----------------------------------------------------------------------------------------
-- Time related types.
-----------------------------------------------------------------------------------------

-- | A timestamp is the amount of seconds since the posix epoch.
newtype Timestamp = Timestamp { _unTimestamp :: POSIXTime }
      deriving (Show, Eq, Ord, Num, Fractional, Real)

-- | A timestamp diff represents the difference in time between two timestamps.
data TimestampDiff = TimestampDiff NominalDiffTime
    deriving (Show)
-----------------------------------------------------------------------------------------

-- ^ A 32 bit number which represents the number of leading 0's that should be in a block header hash.
-- ^ Is dynamically adjusted.
-- TODO: I might have this wrong. Target is the number below which a block header's hash should be.
-- I might need to change this representation.
newtype Target = Target { _unTarget :: Word32 }
      deriving (Show, Integral, Real, Enum, Num, Ord, Eq, Generic)

newtype Nonce = Nonce { _unNonce :: Word32 }
      deriving (Show, Enum, Generic)

newtype MerkleHash = MerkleHash { _unMerkleHash :: Digest SHA256 }
    deriving (Show)

data BlockHeader = BlockHeader
  { _previousBlockHash :: BlockHash
  , _merkleRootHash :: MerkleHash
  , _target :: Target
  , _timestamp :: Timestamp
  , _nonce :: Nonce
  } deriving (Show, Generic)

data Block = Block
  { _blockHeader :: BlockHeader
  , _coinbase :: Transaction
  , _transactions :: [Transaction]
  } deriving (Show)

newtype BlockMap = BlockMap { _unBlockMap :: HM.HashMap BlockHash Block }
  deriving (Show)

data ChainState = ChainState
  { _blocks :: BlockMap
  , _txSet :: TxMap
  } deriving (Show)

newtype ChainT m a = ChainT { _unChainT :: StateT ChainState m a }
  deriving (Functor, Applicative, Monad, MonadState ChainState)

-----------------------------------------------------------------------------------------
-- Transaction related types.
-----------------------------------------------------------------------------------------

-- TODO: Reconsider how I am doing serialization. Do I want to use something like protocol
-- buffers or Apache Thrift?

-- | The amount of tribe coin being transferred in a transaction output.
newtype Amount = Amount { _unAmount :: Word64 }
  deriving (Show, Eq, Generic)

-- | The version of validation rules a transaction should be validated against.
-- This might not be necessary in the long term, but its nice to have just in case.
data TXVersion = TXVersion
  deriving (Show)

-- | Represents the hash of a public key. Obtained by hashing the public key of a user
-- first with sha256 and then with ripemd160.
newtype PubKeyHash = PubKeyHash { _unPubKeyHash :: Digest RIPEMD160 }
  deriving (Show, Eq)

-- ^ The prefix that is prepended to the payment script (e.g. tribe coin address).
newtype Prefix = Prefix { _unPrefix :: Word8 }
  deriving (Show, Eq, Generic)

-- | Version byte prefix for tribe coin addresses.
addressPrefix :: Prefix
addressPrefix = Prefix 0x00

-- | A tribe coin address represents a destination which coin can be sent to.
-- Should be 33 bytes long when serialized.
-- TODO: Remove address checksum. I don't think I need it.
data TribeCoinAddress = TribeCoinAddress
  { _receiverPubKeyHash :: PubKeyHash -- ^ The hash of the public key of the recipient.
  } deriving (Show, Eq)

-- | Represents a transaction output.
data TxOut = TxOut
  { _amount :: Amount -- ^ The amount of coin in this output this can be spent.
  , _receiverAddress :: TribeCoinAddress -- ^ The recipient of the transaction.
  } deriving (Show, Generic)
  
-- | Represents identifier of an unspent transaction in a transaction input.
-- Created by double sha256 hashing the transaction.
newtype TxId = TxId { _unTxId :: Digest SHA256 }
  deriving (Show, Eq)

-- | Represents the index of a specific unspent transaction in a full transaction set.
newtype TxIndex = TxIndex { _unTxIndex :: Word32 }
  deriving (Show, Eq, Generic)

-- | Represents a specific output of a specific transaction.
data Outpoint = Outpoint
  { _txId :: TxId -- ^ The id of the transaction.
  , _txIndex :: TxIndex -- ^ The output index within the transaction.
  } deriving (Show, Eq, Generic)

secp256k1 :: CPET.Curve
secp256k1 = CPET.getCurveByName CPET.SEC_p256k1

newtype PrivKey = PrivKey { _unPrivKey :: ECC.PrivateKey }
  deriving (Show, Eq)

mkPrivKey :: ECC.PrivateNumber -> PrivKey
mkPrivKey num = PrivKey $ ECC.PrivateKey secp256k1 num

privKeyLowerBound :: Integer
privKeyLowerBound = 0x01

privKeyUpperBound :: Integer
privKeyUpperBound = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140

-- | Represents an ECC public key for use in ECDSA.
newtype PubKey = PubKey { _unPubKey :: ECC.PublicKey }
  deriving (Show, Eq)

-- TODO: Take 2 integers
mkPubKey :: ECC.PublicPoint -> PubKey
mkPubKey point = PubKey $ ECC.PublicKey secp256k1 point

-- | Represents a ECDSA signature using the secp256k1 curve with SHA256 hashing.
-- Created by signing all of the transaction inputs and outputs.
newtype Sig = Sig { _unSig :: ECC.Signature }
  deriving (Show)

-- | Represents a message that can be signed or used to verify a signature is valid.
newtype SigMsg = SigMsg { _unSigMsg :: BS.ByteString }
  deriving (Show)

-- | Represents data needed to claim ownership over coins in a specific output.
data SigScript = SigScript
  { _pubKey :: PubKey -- ^ The public key of the owner.
  , _sig :: Sig -- ^ The signature used to prove ownership of the private key corresponding to
                -- ^ the public key.
  } deriving (Show, Generic)

-- | Represents an input to a transaction.
data TxIn = TxIn
  { _prevOutput :: Outpoint -- ^ A specific output of a transaction that will be spent.
  , _sigScript :: SigScript -- ^ Proof of ownership over the coins in |_prevOutput|.
  } deriving (Show)

data Transaction =
    Transaction
      { _inputs :: [TxIn]
      , _outputs :: [TxOut]
      }
  -- | A coinbase transaction has no inputs and awards newly minted coin as its only output.
  | CoinbaseTransaction 
      { _cbOutputs :: TxOut -- ^ The output of the coinbase transaction containing the
                            -- ^ newly minted awarded to the miner of the block.
      } deriving (Show)

newtype TxMap = TxMap { _unTxMap :: HM.HashMap TxId Transaction }
  deriving (Show)

-----------------------------------------------------------------------------------------
-- Typeclass instances.
-----------------------------------------------------------------------------------------

-- | Utility function for serializing a Digest.
putDigest :: HashAlgorithm a
          => Digest a -- ^ The digest to serialize.
          -> Put
putDigest = putByteString . convert

-- | Utility function for deserializing a Digest.
getDigest :: HashAlgorithm a
          => Int -- ^ The amount of bytes to read from the input state.
          -> String -- ^ Error message to print on failure.
          -> Get (Digest a)
getDigest num error_msg = do
  byte_string <- getByteString num
  case digestFromByteString byte_string of
    Nothing       -> MF.fail error_msg
    (Just digest) -> return digest

instance Serialize Target
instance Serialize Nonce
instance Serialize BlockHeader
instance Serialize Amount
instance Serialize Prefix
instance Serialize TxOut
instance Serialize TxIndex
instance Serialize Outpoint
-- TODO: I think this has bugs because both PubKey and Sig use the remaining function.
instance Serialize SigScript

instance Serialize BlockHash where
  put (BlockHash digest) = putDigest digest 
  
  get = BlockHash <$> getDigest 32 "Invalid BlockHash"

instance Serialize Timestamp where
  put (Timestamp time) = put . toRational $ time
  
  get = do
    rational <- get :: (Get Rational)
    return . Timestamp . fromRational $ rational

instance Serialize MerkleHash where
  put (MerkleHash digest) = putDigest digest
  get = MerkleHash <$> getDigest 256 "Invalid MerkleHash"

instance Serialize PubKeyHash where
  put (PubKeyHash digest) = putDigest digest
  
  get = PubKeyHash <$> getDigest 20 "Invalid PubKeyHash"

-- | Calculate the checksum of a tribecoin address. Concatenate the prefix version byte and the hash
-- of the public key, hash the resulting bytestring twice using sha256, and then take the first 4 bytes.
calculateAddressChecksum :: Prefix -> PubKeyHash -> BS.ByteString
calculateAddressChecksum prefix hash =
  let bytes    = (_unPrefix prefix) `BS.cons` (convert . _unPubKeyHash $ hash)
      digest   = convert . hashWith SHA256 . hashWith SHA256 $ bytes
  in BS.take 4 digest

-- | A tribe coin address is serialized by concatentating a version number, the public
-- key hash, and the checksum together as bytes and then base58 encoding the
-- bytestring.
instance Serialize TribeCoinAddress where
  put (TribeCoinAddress hash) = do
    putByteString . encodeBase58 bitcoinAlphabet . runPut $ do
      put addressPrefix
      put hash
      putByteString (calculateAddressChecksum addressPrefix hash)

  get = do
    -- First reverse the base58 encoding.
    bytes <- getByteString 33
    case decodeBase58 bitcoinAlphabet bytes of
      Nothing     -> MF.fail "Invalid base58 encoded TribeCoinAddress."
      Just bytes' -> do
        -- Try to parse the address.
        case parseAddress bytes' of
          Left msg      -> MF.fail msg
          Right address -> return address 
    where parseAddress :: BS.ByteString -> Either String TribeCoinAddress
          parseAddress bs = flip runGet bs $ do
            -- Check the prefix is the right version.
            prefix <- get :: Get Prefix
            case prefix == addressPrefix of
              False -> MF.fail "Invalid TribeCoinAddress Prefix!"
              True  -> do
                -- Verify the checksum is correct.
                hash     <- get :: Get PubKeyHash
                checksum <- getByteString 4
                case checksum == calculateAddressChecksum prefix hash of
                  False -> MF.fail "Invalid TribeCoinAddress checksum for PubKeyHash!"
                  True  -> return $ TribeCoinAddress hash 

instance Serialize TxId where
  put (TxId digest) = putDigest digest
  
  get = TxId <$> getDigest 256 "Invalid TxId"

-- | A private key has a raw format of a 32 byte integer between 0x01 and
-- 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140.
instance Serialize PrivKey where
  put (PrivKey (ECC.PrivateKey _ num)) = putByteString . CNS.i2osp $ num

  get = do
    num <- CNS.os2ip <$> getByteString 32
    if num < privKeyLowerBound || num > privKeyUpperBound then
      MF.fail "Number for PrivKey is out of range!"
    else
      return $ mkPrivKey num

-- | A public key has a raw format of 65 bytes. It starts with a header byte (0x04)
-- and is followed by one 32 byte integer (X coord) and then another 32 byte
-- integer (Y coord).
-- TODO: Support serializing to/from compressed format.
instance Serialize PubKey where
  put (PubKey (ECC.PublicKey _ (CPET.Point x y))) = do
    putWord8 0x04
    putByteString . CNS.i2osp $ x
    putByteString . CNS.i2osp $ y
  -- TODO: This shouldn't ever happen because Point at Inifinity isn't a valid public key vlaue.
  -- Maybe throw an error or something here?
  put (PubKey (ECC.PublicKey _ CPET.PointO)) = undefined
  
  get = do
    has_prefix <- (==) 0x04 <$> getWord8
    case has_prefix of
      False -> MF.fail "Invalid PubKey!"
      True  -> do
        x <- CNS.os2ip <$> getByteString 32
        y <- CNS.os2ip <$> getByteString 32
        return . mkPubKey $ CPET.Point x y

-- | A raw signature consists of two 32 byte integers (r and s).
-- TODO: Verify that the integers are always 32 bytes each.
instance Serialize Sig where
  put (Sig (ECC.Signature r s)) = do
    putByteString . CNS.i2osp $ r
    putByteString . CNS.i2osp $ s
  
  get = do
    r <- CNS.os2ip <$> getByteString 32
    s <- CNS.os2ip <$> getByteString 32
    return . Sig $ ECC.Signature r s
