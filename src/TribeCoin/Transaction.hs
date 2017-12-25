module TribeCoin.Transaction 
  ( verifyPubKey
  ) where

import TribeCoin.Types (TxMap (..), Transaction (..), PubKeyHash (..), SigScript (..), SigMsg (..), PubKey (..), Sig (..))
import TribeCoin.Utils (sha256, ripemd160)

import qualified Crypto.Secp256k1 as ECC (verifySig)
import qualified Data.ByteString as BS (ByteString)
import Data.Serialize (encode)


verifyTx :: Transaction -> TxMap -> Bool
verifyTx tx tx_map = undefined

verifySigScript :: SigScript -> PubKeyHash -> SigMsg -> Bool
verifySigScript script hash msg =
  let is_valid_pubkey = verifyPubKey (_pubKey script) hash
      is_valid_sig = verifySig (_pubKey script) (_sig $ script) msg 
  in is_valid_pubkey && is_valid_sig

-- | Verifies that a public key matches a hash of a public key.
verifyPubKey :: PubKey -> PubKeyHash -> Bool
verifyPubKey pubkey hash =
  -- Hash the public key by first hashing with sha256 and then with ripemd160.
  let hash' = PubKeyHash . ripemd160 . sha256 $ (encode pubkey :: BS.ByteString)
  in hash' == hash

-- | Verifies that a signature is valid given a public key and a message to sign.
verifySig :: PubKey -> Sig -> SigMsg -> Bool
verifySig pubkey sig msg = ECC.verifySig (_unPubKey pubkey) (_unSig sig) (_unSigMsg msg)

