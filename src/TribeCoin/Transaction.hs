module TribeCoin.Transaction 
  ( validateTx
  ) where

import TribeCoin.Types (TxMap (..), Transaction (..))

validateTx :: Transaction -> TxMap -> Bool
validateTx tx tx_map = undefined