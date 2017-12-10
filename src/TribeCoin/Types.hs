module TribeCoin.Types
    (
    ) where

import Crypto.Hash (Digest, SHA256 (..))

type BlockHash = Digest SHA256

data BlockHeader = BlockHeader
  { _previousBlockHash :: BlockHash
  , _timestamp :: ()
  , _difficulty :: ()
  , _nonce :: ()
  }

type Transaction = ()

data Block = Block
  { _blockHeader :: BlockHeader
  , _transactions :: [Transaction]
  }

