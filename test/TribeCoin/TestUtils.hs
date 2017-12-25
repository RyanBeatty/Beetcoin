-- | Contains utility functions and definitions used throughout testing code.
module TribeCoin.TestUtils where

import TribeCoin.Types (PubKey (..), PubKeyHash (..), TribeCoinAddress (..))

import Crypto.Hash (digestFromByteString)
import Crypto.Secp256k1 (importPubKey)
import qualified Data.ByteString as BS (ByteString, pack, append)
import Data.ByteString.Base58 (encodeBase58, bitcoinAlphabet)
import Data.Either (either)
import Data.Maybe (fromJust)
import Data.Serialize (encode, decode)
import Data.Word (Word32, Word8)

-- | Byte representation of a private key. Taken from
-- https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
rawPrivateKey :: BS.ByteString
rawPrivateKey = BS.pack 
  [ 0x18, 0xE1, 0x4A, 0x7B, 0x6A, 0x30, 0x7F, 0x42, 0x6A, 0x94, 0xF8, 0x11, 0x47, 0x01, 0xE7, 0xC8
  , 0xE7, 0x74, 0xE7, 0xF9, 0xA4, 0x7E, 0x2C, 0x20, 0x35, 0xDB, 0x29, 0xA2, 0x06, 0x32, 0x17, 0x25
  ]

-- | Byte representation of a public key. Taken from
-- https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
rawPubKey :: BS.ByteString
rawPubKey = BS.pack
  [ 0x04, 0x50, 0x86, 0x3A, 0xD6, 0x4A, 0x87, 0xAE, 0x8A, 0x2F, 0xE8, 0x3C, 0x1A, 0xF1, 0xA8, 0x40
  , 0x3C, 0xB5, 0x3F, 0x53, 0xE4, 0x86, 0xD8, 0x51, 0x1D, 0xAD, 0x8A, 0x04, 0x88, 0x7E, 0x5B, 0x23
  , 0x52, 0x2C, 0xD4, 0x70, 0x24, 0x34, 0x53, 0xA2, 0x99, 0xFA, 0x9E, 0x77, 0x23, 0x77, 0x16, 0x10
  , 0x3A, 0xBC, 0x11, 0xA1, 0xDF, 0x38, 0x85, 0x5E, 0xD6, 0xF2, 0xEE, 0x18, 0x7E, 0x9C, 0x58, 0x2B
  , 0xA6
  ]

-- | Byte representation of a public key hash. Taken from
-- https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
rawPubKeyHash :: BS.ByteString
rawPubKeyHash = BS.pack 
  [ 0x01, 0x09, 0x66, 0x77, 0x60, 0x06, 0x95
  , 0x3D, 0x55, 0x67, 0x43, 0x9E, 0x5E, 0x39
  , 0xF8, 0x6A, 0x0D, 0x27, 0x3B, 0xEE
  ]

-- | Byte representation for the checksum for an address. Taken from
-- https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
rawAddressChecksum :: BS.ByteString
rawAddressChecksum = encode (0xD61967F6 :: Word32)

-- | Raw version byte for an address. Taken from
-- https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
rawVersionByte :: BS.ByteString
rawVersionByte = encode (0x00 :: Word8)

-- | Byte representation of an address. Taken from
-- https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
rawTribeCoinAddress :: BS.ByteString
rawTribeCoinAddress = encodeBase58 bitcoinAlphabet $ rawVersionByte `BS.append` rawPubKeyHash `BS.append` rawAddressChecksum

parsedPubKey :: PubKey
parsedPubKey = PubKey . fromJust . importPubKey $ rawPubKey

parsedPubKeyHash :: PubKeyHash
parsedPubKeyHash = PubKeyHash . fromJust . digestFromByteString $ rawPubKeyHash

parsedTribeCoinAddress :: TribeCoinAddress
parsedTribeCoinAddress = either (error "Failed to parse raw TribeCoin address!") (id) . decode $ rawTribeCoinAddress