module BeetCoin.Core.TransactionTest where

import BeetCoin.Core.Transaction
import BeetCoin.Core.Types (PubKey (..))

import BeetCoin.Core.Arbitrary (RandomPubKeyHash (..))
import BeetCoin.Core.TestUtils (parsedPubKey, parsedPubKeyHash, parsedSig, parsedSigMsg)

import Test.Tasty.Hspec (Spec, it, describe)
import Test.Tasty.QuickCheck (Property, forAll, arbitrary, suchThat)


spec_VerifyPubKey :: Spec
spec_VerifyPubKey = do
  describe "verifyPubKey Tests" $ do
    -- Verify that a public key will hash to the correct public key hash.
    it "Public Key matches Public Key Hash" $ do
      verifyPubKey parsedPubKey parsedPubKeyHash == True

-- TODO: Maybe turn this into a quickcheck test where a random SigMsg is generated
-- and then signed by a priv key.
spec_VerifySig :: Spec
spec_VerifySig = do
  describe "verifySig Tests" $ do
    -- Verify that a signature that was signed by a private key can be verified with the
    -- original unsigned message and corresponding public key.
    it "PubKey validates signed message" $ do
      verifySig parsedPubKey parsedSig parsedSigMsg == True

-- | Verifies that a public key will never hash to a public key hash that it doesn not match.
prop_PubKeyAndPubKeyHashMismatch :: Property
prop_PubKeyAndPubKeyHashMismatch =
  -- Make sure we generate a public key hash that does not match the hash of this public
  -- key.
  forAll (arbitrary `suchThat` \h -> (_unRandomPubKeyHash h) /= parsedPubKeyHash) $
    \hash -> verifyPubKey parsedPubKey (_unRandomPubKeyHash hash) == False
