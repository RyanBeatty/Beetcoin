module TribeCoin.Blocks
    ( hashBlockHeader
    ) where

import TribeCoin.Types (Block (..), BlockHeader (..), BlockHash (..), Nonce (..))

import Data.ByteString as BS (ByteString, take, replicate)
import Data.ByteString.Conversion (toByteString')
import Data.Serialize (encode)
import Crypto.Hash (hash)

-- | Lazy list of all possible nonce values.
-- TODO: check if this is the right bounds.
nonces :: [Nonce]
nonces = [Nonce 0..]

hashBlockHeader ::
  BlockHeader  -- ^ The block header to hash using sha256.
  -> BlockHash
hashBlockHeader = BlockHash . hash . encode

tryMineBlock :: 
  BlockHeader -- ^ The block header to try hashing.
  -> Int      -- ^ The number of leading 0's that should be in the hashed block header. TODO: Figure out what values are legal.
  -> Bool
tryMineBlock block_header difficulty =
  let block_hash :: BS.ByteString
      block_hash = encode . hashBlockHeader $ block_header
  in BS.take difficulty block_hash == BS.replicate difficulty 0
