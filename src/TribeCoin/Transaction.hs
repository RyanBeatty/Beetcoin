module TribeCoin.Transaction 
  (
  ) where

import TribeCoin.Types (TxMap (..), Transaction (..), PubKeyHash (..), SigScript (..))
import TribeCoin.Utils (sha256, ripemd160)

import Data.ByteArray (convert)
import Data.Serialize (encode)


verifyTx :: Transaction -> TxMap -> Bool
verifyTx tx tx_map = undefined

verifySigScript :: SigScript -> PubKeyHash -> Bool
verifySigScript script hash =
  -- Hash the public key by first hashing with sha256 and then with ripemd160.
  let hash' = PubKeyHash . ripemd160 . convert . sha256 . encode . _pubKey $ script
  in hash' == hash

