-- | Contains utility functions and definitions used throughout testing code.
module TribeCoin.TestUtils where

import TribeCoin.Types

import Crypto.Hash (digestFromByteString)
import Crypto.PubKey.ECC.ECDSA (PublicKey (..), PrivateKey (..), Signature (..))
import Crypto.PubKey.ECC.Types (Point (..))
import qualified Data.ByteString as BS (ByteString, pack, append)
import Data.ByteString.Base58 (encodeBase58, bitcoinAlphabet)
import Data.Either (either)
import Data.Maybe (fromJust)
import Data.Serialize (encode, decode)
import Data.Word (Word32, Word8)

-- | Byte representation of a private key. Taken from
-- https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
rawPrivKey :: BS.ByteString
rawPrivKey = BS.pack 
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

rawSig :: BS.ByteString
rawSig = BS.pack
  [ 0x30, 0x46, 0x02, 0x21, 0x00, 0x82, 0x6b, 0x99, 0x8d, 0x41, 0x8a, 0x96, 0x1f, 0x6b, 0x56, 0xed
  , 0x94, 0x6d, 0xb3, 0x92, 0x3c, 0xa6, 0xdd, 0x33, 0xd4, 0xcd, 0x11, 0xd2, 0xda, 0x4c, 0x44, 0xcb
  , 0xb1, 0x22, 0xb1, 0x63, 0x77, 0x02, 0x21, 0x00, 0xff, 0xb6, 0xe5, 0x93, 0xf2, 0x12, 0x0a, 0x4d
  , 0x58, 0xf3, 0x35, 0x71, 0x85, 0x4a, 0xba, 0xa9, 0xd1, 0x7c, 0x2f, 0xcd, 0x77, 0x40, 0x76, 0xcd
  , 0x63, 0x70, 0x40, 0x87, 0x68, 0x8a, 0x13, 0xf4
  ]

-- | Byte representation of an address. Taken from
-- https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
rawTribeCoinAddress :: BS.ByteString
rawTribeCoinAddress = encodeBase58 bitcoinAlphabet $ rawVersionByte `BS.append` rawPubKeyHash `BS.append` rawAddressChecksum

parsedPrivKey :: PrivKey
parsedPrivKey = mkPrivKey 0x18E14A7B6A307F426A94F8114701E7C8E774E7F9A47E2C2035DB29A206321725

-- | Parsed representation of a public key. Built from the x and y coords of the raw public key.
parsedPubKey :: PubKey
parsedPubKey = mkPubKey (Point 0x50863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B2352
                               0x2CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6)

-- | Parsed representation of a public key hash.
parsedPubKeyHash :: PubKeyHash
parsedPubKeyHash = PubKeyHash . fromJust . digestFromByteString $ rawPubKeyHash

-- | Parsed representation of a tribecoin address.
parsedTribeCoinAddress :: TribeCoinAddress
parsedTribeCoinAddress = TribeCoinAddress parsedPubKeyHash

-- | Parsed representation of a signed message. The string "The quick, brown fox jumps over the lazy dog." was
-- encoded as a bytestring and then signed using the cryptonite ECDSA functions.
parsedSig :: Sig
parsedSig = Sig $ Signature 78699570603611690428924495811105138185150196139902674087121660046572889070950
                            46020018641752249994803304601687045467005478091757143026168859170816958885749

parsedSigMsg :: SigMsg
parsedSigMsg = SigMsg . encode $ "The quick, brown fox jumps over the lazy dog."
