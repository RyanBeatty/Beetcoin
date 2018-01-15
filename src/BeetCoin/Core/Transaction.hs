module BeetCoin.Core.Transaction 
  ( validateTransactions
  , verifyPubKey
  , verifySig
  ) where

import BeetCoin.Core.Types
  ( UtxoMap (..), Transaction (..), PubKeyHash (..), SigScript (..), SigMsg (..), PubKey (..), Sig (..)
  , Outpoint (..), TxOut (..), Amount (..), TxIn (..), Utxo (..)
  )
import BeetCoin.Core.Utils (sha256, ripemd160)

import Crypto.Hash (SHA256 (..))
import qualified Crypto.PubKey.ECC.ECDSA as ECC (verify)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.HashMap.Strict as HM (lookup)
import Data.Serialize (encode)

validateTransactions = True


verifyTx :: Transaction -> UtxoMap -> Bool
verifyTx tx tx_map = undefined

verifyTxOuts :: [TxOut] -> Amount -> Bool
verifyTxOuts [] available = available == 0
verifyTxOuts (output:os) available
  -- This check avoids overflow/underflow issues.
  | (_amount output) > available = False
  | otherwise                    = verifyTxOuts os (available - (_amount output))

verifyTxIn :: TxIn -> PubKeyHash -> SigMsg -> UtxoMap -> Either String Amount
verifyTxIn input hash msg map
  | verifySigScript (_sigScript input) hash msg = verifyOutpoint (_prevOutput input) map
  | otherwise                                   = Left "Failed to verify SigScript!"

verifyOutpoint :: Outpoint -> UtxoMap -> Either String Amount
verifyOutpoint outpoint utxos = undefined

-- | Verifies that a sig script is fulfilled.
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
verifySig pubkey sig msg = ECC.verify SHA256 (_unPubKey pubkey) (_unSig sig) (_unSigMsg msg)
