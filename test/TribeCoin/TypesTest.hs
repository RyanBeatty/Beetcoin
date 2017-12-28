module TribeCoin.TypesTest where

import TribeCoin.Types
import TribeCoin.TestUtils

import Crypto.Hash (digestFromByteString)
import qualified Data.ByteString as BS (ByteString, pack, length)
import Data.ByteString.Base58 (encodeBase58, bitcoinAlphabet)
import Data.Maybe (fromJust)
import Data.Serialize (encode, decode)
import Data.Word (Word32)
import qualified Test.Tasty.Hspec as HS

spec_PubKeyHash :: HS.Spec
spec_PubKeyHash = do
  HS.describe "PubKeyHash Tests" $ do

    HS.describe "Encoding Tests" $ do
      -- Verify that an encoded public key hash is the same as its raw representation.
      HS.it "Encoding Works" $ do
        encode parsedPubKeyHash `HS.shouldBe` rawPubKeyHash
      
      -- Makes sure that the encoded public key hash is 20 bytes long.
      HS.it "Encoded Length" $ do
        (BS.length . encode $ parsedPubKeyHash) `HS.shouldBe` 20

    HS.describe "Decoding Tests" $ do
      -- Verify that decoding a raw public key results in the correct internal representation.
      HS.it "Decode Works" $ do
        decode rawPubKeyHash `HS.shouldBe` Right parsedPubKeyHash

spec_TribeCoinAddress :: HS.Spec
spec_TribeCoinAddress = do
  HS.describe "TribeCoinAddress" $ do
    
    HS.describe "Encoding Tests" $ do
      -- Verify that an encoded tribe coin address is the same as its raw representation.
      HS.it "Encoding Works" $ do
        encode parsedTribeCoinAddress `HS.shouldBe` rawTribeCoinAddress

      -- Verify that an encoded tribe coin address has a length of 33 bytes.
      -- TODO: Make sure 33 is the correct number here. It should be 25 bytes before it is
      -- base58 encoded.
      HS.it "Encoded Length" $ do
        (BS.length . encode $ parsedTribeCoinAddress) `HS.shouldBe` 33
  
    HS.describe "Decoding Tests" $ do
      -- Verify that a decoded raw tribe coin address matches its internal representation.
      HS.it "Decode Works" $ do
        decode rawTribeCoinAddress `HS.shouldBe` Right parsedTribeCoinAddress 