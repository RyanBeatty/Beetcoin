module TribeCoin.TransactionTest where

import TribeCoin.Arbitrary (RandomPubKeyHash (..))
import TribeCoin.TestUtils (parsedPubKey, parsedPubKeyHash)

import TribeCoin.Transaction (verifyPubKey)
import TribeCoin.Types (PubKey (..))

import Test.Tasty.QuickCheck (Property, forAll, arbitrary, suchThat)


-- | Verifies that a public key will never hash to a public key hash that it doesn not match.
prop_pubKeyAndPubKeyHashMismatch :: Property
prop_pubKeyAndPubKeyHashMismatch =
  -- Make sure we generate a public key hash that does not match the hash of this public
  -- key.
  forAll (arbitrary `suchThat` \h -> (_unRandomPubKeyHash h) /= parsedPubKeyHash) $
    \hash -> verifyPubKey parsedPubKey (_unRandomPubKeyHash hash) == False


