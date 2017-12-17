{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module TribeCoin.Types
    ( BlockHash (..)
    , Timestamp (..)
    , TimestampDiff (..)
    , Difficulty (..)
    , Nonce (..)
    , BlockHeader (..)
    , Block (..)
    ) where

import Control.Monad.Fail as MF (fail)
import Crypto.Hash (Digest, SHA256, RIPEMD160, digestFromByteString)
import Data.ByteArray (ByteArrayAccess, convert)
import qualified Data.ByteString as BS (ByteString, append)
import Data.ByteString.Base58 (bitcoinAlphabet, encodeBase58, decodeBase58)
import Data.Serialize (Serialize, put, get, Get, Putter, encode)
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

instance Serialize BlockHash where
  put (BlockHash digest) = put $ (convert digest :: BS.ByteString)
  get = do
    byte_string <- get :: (Get BS.ByteString)
    case digestFromByteString byte_string of
      Nothing       -> MF.fail "Invalid BlockHash"
      (Just digest) -> return $ BlockHash digest

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
  deriving (Show)

-- | The checksum used to verify a public key hash has not been altered. Obtained by
-- taking the first 4 bytes of an address version + public key hash after furthering
-- hashing it twice with sha256.
newtype AddressChecksum = AddressChecksum Word32
  deriving (Show, Generic)

instance Serialize AddressChecksum

-- | The version of validation rules a transaction should be validated against.
-- This might not be necessary in the long term, but its nice to have just in case.
data TXVersion = TXVersion

instance Show TXVersion where
  show TXVersion = showHex 1 ""

-- | Represents the hash of a public key. Obtained by hashing the public key of a user
-- first with sha256 and then with ripemd160.
newtype PubKeyHash = PubKeyHash (Digest RIPEMD160)
  deriving (Show)

-- TODO: See if I can share code between BlockHash's Serialize instance.
instance Serialize PubKeyHash where
  put (PubKeyHash digest) = put $ (convert digest :: BS.ByteString)
  get = do
    byte_string <- get :: (Get BS.ByteString)
    case digestFromByteString byte_string of
      Nothing       -> MF.fail "Invalid PubKeyHash"
      (Just digest) -> return $ PubKeyHash digest

addressPrefix :: Word32
addressPrefix = 1

-- | A tribe coin address represents a destination which coin can be sent to.
data TribeCoinAddress = TribeCoinAddress
  { _receiverPubKeyHash :: PubKeyHash -- ^ The hash of the public key of the recipient.
  , _checksum :: AddressChecksum -- ^ checksum for the version + public key hash.
  }

instance Show TribeCoinAddress where
  show (TribeCoinAddress hash checksum) = "" 

instance Serialize TribeCoinAddress where
  put (TribeCoinAddress hash checksum) =
    let version'  = encode addressPrefix
        hash'     = encode hash
        checksum' = encode checksum
        bytes     = version' `BS.append` hash' `BS.append` checksum'
    in put . encodeBase58 bitcoinAlphabet $ bytes 

  get = do
    decodeBase58 bitcoinAlphabet <$> get
    _ <- get :: Get Word32
    hash <- get :: Get PubKeyHash
    checksum <- get :: Get AddressChecksum
    return $ TribeCoinAddress hash checksum

data TXOut = TXOut
  { _amount :: Amount
  , _receiverAddress :: TribeCoinAddress
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

