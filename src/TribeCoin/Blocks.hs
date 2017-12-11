module TribeCoin.Blocks
    ( hashBlockHeader
    ) where

import TribeCoin.Types (Block (..), BlockHeader (..), BlockHash (..))

import Data.ByteString as BS (ByteString, take, replicate)
import Data.ByteString.Conversion (toByteString')
import Data.Serialize (encode)
import Crypto.Hash (hash)

hashBlockHeader :: BlockHeader -> BlockHash
hashBlockHeader = BlockHash . hash . encode

tryMineBlock :: BlockHeader -> Bool
tryMineBlock block_header =
  let block_hash :: BS.ByteString
      block_hash = encode . hashBlockHeader $ block_header
      difficulty :: Int
      difficulty = fromIntegral . _difficulty $ block_header
  in BS.take difficulty block_hash < BS.replicate difficulty 0
