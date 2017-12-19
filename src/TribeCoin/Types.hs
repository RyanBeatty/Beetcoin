{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module TribeCoin.Types
    ( BlockHash (..)
    , Timestamp (..)
    , TimestampDiff (..)
    , Difficulty (..)
    , Nonce (..)
    , BlockHeader (..)
    , Block (..)
    , Amount (..)
    , AddressChecksum (..)
    , TXVersion (..)
    , PubKeyHash (..)
    , TribeCoinAddress (..)
    ) where

import Control.Monad.Fail as MF (fail)
import qualified Crypto.Secp256k1 as ECC 
  ( PubKey, Sig, exportPubKey
  , importPubKey, importSig, exportSig
  )
import Crypto.Hash (Digest, SHA256, RIPEMD160, HashAlgorithm, digestFromByteString)
import Crypto.PubKey.ECC.ECDSA (PublicKey)
import Data.ByteArray (ByteArrayAccess, convert)
import qualified Data.ByteString as BS (ByteString, append, length)
import Data.ByteString.Base58 (bitcoinAlphabet, encodeBase58, decodeBase58)
import Data.Serialize
  ( Serialize, Get, Putter, Put, put, get, encode
  , runPut, runGet, putByteString, remaining, getByteString
  )
import Data.Time (NominalDiffTime (..))
import Data.Time.Clock.POSIX (POSIXTime)
import Data.UnixTime (UnixTime (..))
import Data.Word (Word32, Word64, Word8)
import Foreign.C.Types (CTime (..))
import GHC.Generics (Generic)
import Numeric (showHex)

-- ^ The sha256 hash of a block header.
newtype BlockHash = BlockHash (Digest SHA256)
      deriving (Show, Eq)

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

instance Serialize BlockHash where
  put (BlockHash digest) = putDigest digest 
  
  get = BlockHash <$> getDigest 32 "Invalid BlockHash"

-----------------------------------------------------------------------------------------
-- Time related types.
-----------------------------------------------------------------------------------------

-- | A timestamp is the amount of seconds since the posix epoch.
newtype Timestamp = Timestamp POSIXTime
      deriving (Show, Eq, Ord, Num, Fractional, Real)

instance Serialize Timestamp where
  put (Timestamp time) = put . toRational $ time
  
  get = do
    rational <- get :: (Get Rational)
    return . Timestamp . fromRational $ rational

-- | A timestamp diff represents the difference in time between two timestamps.
data TimestampDiff = TimestampDiff NominalDiffTime
    deriving (Show)
-----------------------------------------------------------------------------------------

-- ^ A 32 bit number which represents the number of leading 0's that should be in a block header hash.
-- ^ Is dynamically adjusted.
-- TODO: I might have this wrong. Target is the number below which a block header's hash should be.
-- I might need to change this representation.
newtype Difficulty = Difficulty Word32
      deriving (Show, Integral, Real, Enum, Num, Ord, Eq, Generic)
instance Serialize Difficulty

newtype Nonce = Nonce Word32
      deriving (Show, Enum, Generic)
instance Serialize Nonce

data BlockHeader = BlockHeader
  { _previousBlockHash :: BlockHash
  , _difficulty :: Difficulty
  , _timestamp :: Timestamp
  , _nonce :: Nonce
  } deriving (Show, Generic)
instance Serialize BlockHeader

-----------------------------------------------------------------------------------------
-- Transaction related types.
-----------------------------------------------------------------------------------------

-- | The amount of tribe coin being transferred in a transaction output.
newtype Amount = Amount Word64
  deriving (Show, Eq, Generic)

instance Serialize Amount

-- | The checksum used to verify a public key hash has not been altered. Obtained by
-- taking the first 4 bytes of an address version + public key hash after furthering
-- hashing it twice with sha256.
newtype AddressChecksum = AddressChecksum Word32
  deriving (Show, Eq, Generic)

instance Serialize AddressChecksum

-- | The version of validation rules a transaction should be validated against.
-- This might not be necessary in the long term, but its nice to have just in case.
data TXVersion = TXVersion
  deriving (Show)

-- | Represents the hash of a public key. Obtained by hashing the public key of a user
-- first with sha256 and then with ripemd160.
newtype PubKeyHash = PubKeyHash (Digest RIPEMD160)
  deriving (Show, Eq)

instance Serialize PubKeyHash where
  put (PubKeyHash digest) = putDigest digest
  
  get = PubKeyHash <$> getDigest 20 "Invalid PubKeyHash"

-- ^ The prefix that is prepended to the payment script (e.g. tribe coin address).
newtype Prefix = Prefix Word8
  deriving (Show, Generic)

instance Serialize Prefix

-- | Version byte prefix for tribe coin addresses.
addressPrefix :: Prefix
addressPrefix = Prefix 0x00

-- | A tribe coin address represents a destination which coin can be sent to.
-- Should be 33 bytes long when serialized.
data TribeCoinAddress = TribeCoinAddress
  { _receiverPubKeyHash :: PubKeyHash -- ^ The hash of the public key of the recipient.
  , _checksum :: AddressChecksum -- ^ checksum for the version + public key hash.
  } deriving (Show, Eq)

instance Serialize TribeCoinAddress where
  -- | A tribe coin address is serialized by concatentating a version number, the public
  -- key hash, and the checksum together as bytes and then base58 encoding the
  -- bytestring.
  put (TribeCoinAddress hash checksum) = do
    putByteString . encodeBase58 bitcoinAlphabet . runPut $ do
      put addressPrefix
      put hash
      put checksum

  get = do
    -- First reverse the base58 encoding.
    bytes <- getByteString 33
    case decodeBase58 bitcoinAlphabet bytes of
      Nothing     -> MF.fail "Invalid base58 encoded TribeCoinAddress."
      Just bytes' ->
        -- Then try and parse the address.
        let parseAddress :: BS.ByteString -> Either String TribeCoinAddress
            parseAddress = \b -> flip runGet b $ do
              _        <- get :: Get Prefix
              hash     <- get :: Get PubKeyHash
              checksum <- get :: Get AddressChecksum
              return $ TribeCoinAddress hash checksum
        in case parseAddress bytes' of
          Left error    -> MF.fail error
          Right address -> return address

-- | Represents a transaction output.
data TxOut = TxOut
  { _amount :: Amount -- ^ The amount of coin in this output this can be spent.
  , _receiverAddress :: TribeCoinAddress -- ^ The recipient of the transaction.
  } deriving (Show, Generic)
  
instance Serialize TxOut

-- | Represents identifier of an unspent transaction in a transaction input.
-- Created by double sha256 hashing the transaction.
newtype TxId = TxId (Digest SHA256)
  deriving (Show, Eq)

instance Serialize TxId where
  put (TxId digest) = putDigest digest
  
  get = TxId <$> getDigest 256 "Invalid TxId"

-- | Represents the index of a specific unspent transaction in a full transaction set.
newtype TXIndex = TXIndex Word32
  deriving (Show, Eq, Generic)

instance Serialize TXIndex

-- | Represents a specific output of a specific transaction.
data Outpoint = Outpoint
  { _txId :: TxId -- ^ The id of the transaction.
  , _txIndex :: TXIndex -- ^ The output index within the transaction.
  } deriving (Show, Eq, Generic)

instance Serialize Outpoint

-- | Represents a public key used in ECDSA with curve secp256k1.
newtype PubKey = PubKey ECC.PubKey
  deriving (Show)

-- TODO: Support serializing to/from compressed format.
instance Serialize PubKey where
  put (PubKey pubkey) = putByteString . ECC.exportPubKey False $ pubkey
  
  get = do
    -- public keys are 65 bytes long (id byte + two 32 byte integers).
    bytes <- getByteString 65
    case ECC.importPubKey bytes of
      Nothing     -> MF.fail "Invalid DER encoded PubKey."
      Just pubkey -> return . PubKey $ pubkey

-- | Represents a ECDSA signature using the secp256k1 curve. Serialized as a DER
-- encoded bytestring.
newtype Sig = Sig ECC.Sig
  deriving (Show)

instance Serialize Sig where
  put (Sig sig) = putByteString . ECC.exportSig $ sig

  get = do
    bytes <- remaining >>= \n -> getByteString n
    case ECC.importSig bytes of
      Nothing  -> MF.fail "Invalid DER encoded Signature."
      Just sig -> return . Sig $ sig

-- | Represents data needed to claim ownership over coins in a specific output.
data SigScript = SigScript
  { _pubKey :: PubKey -- ^ The public key of the owner.
  , _sig :: Sig -- ^ The signature used to prove ownership of the private key corresponding to
                -- ^ the public key.
  } deriving (Show, Generic)

-- TODO: I think this has bugs because both PubKey and Sig use the remaining function.
instance Serialize SigScript where

-- | Represents an input to a transaction.
data TxIn = TxIn
  { _prevOutput :: Outpoint -- ^ A specific output of a transaction that will be spent.
  , _sigScript :: SigScript -- ^ Proof of ownership over the coins in |_prevOutput|.
  } deriving (Show)

data Transaction = Transaction
  { _txVersion :: TXVersion
  , _sender :: ()
  , _receiver :: ()
  } deriving (Show)

data Block = Block
  { _blockHeader :: BlockHeader
  , _transactions :: [Transaction]
  } deriving (Show)

