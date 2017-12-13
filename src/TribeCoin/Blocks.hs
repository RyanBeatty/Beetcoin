module TribeCoin.Blocks
    ( mineBlock
    ) where

import TribeCoin.Types (Block (..), BlockHeader (..), BlockHash (..), Nonce (..))

import Data.ByteString as BS (ByteString, take, replicate)
import Data.ByteString.Conversion (toByteString')
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Serialize (encode)
import Crypto.Hash (hash)

-- | Hash a block header using sha256.
hashBlockHeader ::
  BlockHeader  -- ^ The block header to hash using sha256.
  -> BlockHash
hashBlockHeader = BlockHash . hash . encode

-- | Lazy list of all possible nonce values.
-- TODO: check if this is the right bounds. Also see if this gets garbage collected ever.
-- If this is strictly evaluated and then sticks around in memory, We'll run out of memory real quick.
nonces :: [Nonce]
nonces = [Nonce 0..]

-- | Lazy list of all possible block headers with their nonces given a block header without a proof of work.
blocks ::
  BlockHeader -- ^ A block header that doesn't have a valid proof of work.
  -> [(Nonce, BlockHeader)]
blocks block_header = fmap (\n -> (n, block_header { _nonce = n })) nonces

-- | Returns a lazy list of all of the prefixes for all possible hashes of a block header along with the nonce used to generate the prefix.
blockHashPrefixes ::
  BlockHeader -- ^ The block header to hash.
  -> Int      -- ^ The lenght of the hash prefix we want.
  -> [(Nonce, BS.ByteString)]
blockHashPrefixes block_header difficulty = (fmap . fmap) (BS.take difficulty . encode . hashBlockHeader) . blocks $ block_header

-- | Calculates the proof of work for a block header.
mineBlock ::
  BlockHeader -- ^ The block header without a proof of work.
  -> BlockHeader
mineBlock block_header =
  let difficulty = fromIntegral . _difficulty $ block_header
      prefix    = BS.replicate difficulty 0
      -- Find a nonce with a resulting block header hash prefix that is equal to the string of leading 0's needed to satisfy the difficulty target.
      -- TODO: Using fromJust is probably a bad idea here. Maybe use an error monad or something.
      (nonce, _) = fromJust . find ((==) prefix . snd) . blockHashPrefixes block_header $ difficulty
  in block_header { _nonce = nonce }
