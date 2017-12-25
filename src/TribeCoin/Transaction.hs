module TribeCoin.Transaction 
  (
  ) where

import TribeCoin.Types (TxMap (..), Transaction (..), PubKeyHash (..), SigScript (..), SigMsg (..), PubKey (..))
import TribeCoin.Utils (sha256, ripemd160)

import qualified Crypto.Secp256k1 as ECC (verifySig)
import qualified Data.ByteString as BS (ByteString)
import Data.Serialize (encode)


verifyTx :: Transaction -> TxMap -> Bool
verifyTx tx tx_map = undefined

verifySigScript :: SigScript -> PubKeyHash -> SigMsg -> Bool
verifySigScript script hash msg =
  -- Hash the public key by first hashing with sha256 and then with ripemd160.
  let pub_key = _pubKey script
      hash' = PubKeyHash . ripemd160 . sha256 $ (encode pub_key :: BS.ByteString)
  in hash' == hash

