module TribeCoin.TypesTest where

import TribeCoin.Types

import Crypto.Hash (digestFromByteString)
import qualified Data.ByteString as BS (ByteString, pack, length)
import Data.ByteString.Base58 (encodeBase58, bitcoinAlphabet)
import Data.Maybe (fromJust)
import Data.Serialize (encode, decode)
import Data.Word (Word32)
import qualified Test.Tasty.Hspec as HS

rawPubKeyHash :: BS.ByteString
rawPubKeyHash = BS.pack 
  [ 0x01, 0x09, 0x66, 0x77, 0x60, 0x06, 0x95
  , 0x3D, 0x55, 0x67, 0x43, 0x9E, 0x5E, 0x39
  , 0xF8, 0x6A, 0x0D, 0x27, 0x3B, 0xEE
  ]

pubKeyHash :: PubKeyHash
pubKeyHash = PubKeyHash . fromJust . digestFromByteString $ rawPubKeyHash

spec_PubKeyHash :: HS.Spec
spec_PubKeyHash = do
  HS.describe "PubKeyHash Tests" $ do
    HS.describe "Encoding Tests" $ do
      -- Verify that an encoded public key hash is the same as its raw representation.
      HS.it "Encoding Works" $ do
        encode pubKeyHash `HS.shouldBe` rawPubKeyHash
      
      -- Makes sure that the encoded public key hash is 20 bytes long.
      HS.it "Encoded Length" $ do
        (BS.length . encode $ pubKeyHash) `HS.shouldBe` 20

    HS.describe "Decoding Tests" $ do
      -- Verify that decoding a raw public key results in the correct internal representation.
      HS.it "Decode Works" $ do
        decode rawPubKeyHash `HS.shouldBe` Right pubKeyHash


rawTribeCoinAddress :: BS.ByteString
rawTribeCoinAddress = encodeBase58 bitcoinAlphabet . BS.pack $
  [ 0x00, 0x01, 0x09, 0x66, 0x77, 0x60, 0x06
  , 0x95, 0x3D, 0x55, 0x67, 0x43, 0x9E, 0x5E
  , 0x39, 0xF8, 0x6A, 0x0D, 0x27, 0x3B, 0xEE
  , 0xD6, 0x19, 0x67, 0xF6
  ]

spec_TribeCoinAddress :: HS.Spec
spec_TribeCoinAddress = do
  HS.describe "TribeCoinAddress" $ do
    let checksum = AddressChecksum (0xD61967F6 :: Word32)
    let address  = TribeCoinAddress pubKeyHash checksum
    
    HS.describe "Encoding Tests" $ do
      -- Verify that an encoded tribe coin address is the same as its raw representation.
      HS.it "Encoding Works" $ do
        encode address `HS.shouldBe` rawTribeCoinAddress

      -- Verify that an encoded tribe coin address has a length of 33 bytes.
      -- TODO: Make sure 33 is the correct number here. It should be 25 bytes before it is
      -- base58 encoded.
      HS.it "Encoded Length" $ do
        (BS.length . encode $ address) `HS.shouldBe` 33
  
    HS.describe "Decoding Tests" $ do
      -- Verify that a decoded raw tribe coin address matches its internal representation.
      HS.it "Decode Works" $ do
        decode rawTribeCoinAddress `HS.shouldBe` Right address 