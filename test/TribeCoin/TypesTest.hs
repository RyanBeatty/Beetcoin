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
  [0x01, 0x09, 0x66, 0x77, 0x60, 0x06, 0x95,
   0x3D, 0x55, 0x67, 0x43, 0x9E, 0x5E, 0x39,
   0xF8, 0x6A, 0x0D, 0x27, 0x3B, 0xEE
  ]

pubKeyHash :: PubKeyHash
pubKeyHash = PubKeyHash . fromJust . digestFromByteString $ rawPubKeyHash

spec_PubKeyHash :: HS.Spec
spec_PubKeyHash = do
  HS.describe "PubKeyHash Tests" $ do
    HS.describe "Encoding Tests" $ do
      HS.it "Encoding Works" $ do
        encode pubKeyHash `HS.shouldBe` rawPubKeyHash

      HS.it "Encoded Length" $ do
        (BS.length . encode $ pubKeyHash) `HS.shouldBe` 20


-- spec_TribeCoinAddress :: HS.Spec
-- spec_TribeCoinAddress = do
--   HS.describe "TribeCoinAddress" $ do
--     HS.it "Encoding works" $ do
--       let hash     = PubKeyHash . fromJust . digestFromByteString . BS.pack $ [0x01, 0x09, 0x66, 0x77, 0x60, 0x06, 0x95, 0x3D, 0x55, 0x67, 0x43, 0x9E, 0x5E, 0x39, 0xF8, 0x6A, 0x0D, 0x27, 0x3B, 0xEE]
--       let checksum = AddressChecksum (0xD61967F6 :: Word32)
--       let address  = TribeCoinAddress hash checksum
--       (encode $ address) `HS.shouldBe` (encodeBase58 bitcoinAlphabet . BS.pack $ [0x00, 0x01, 0x09, 0x66, 0x77, 0x60, 0x06, 0x95, 0x3D, 0x55, 0x67, 0x43, 0x9E, 0x5E, 0x39, 0xF8, 0x6A, 0x0D, 0x27, 0x3B, 0xEE, 0xD6, 0x19, 0x67, 0xF6])