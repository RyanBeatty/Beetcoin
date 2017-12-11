module TribeCoin.Blocks
    ( hashBlockHeader
    ) where

import TribeCoin.Types (Block (..), BlockHeader (..), BlockHash (..))

import Data.Serialize (encode)
import Crypto.Hash (hash)

hashBlockHeader :: BlockHeader -> BlockHash
hashBlockHeader = BlockHash . hash . encode
