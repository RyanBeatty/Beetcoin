module BeetCoin.Blocks
    ( mineBlock
    ) where

import BeetCoin.Types
  ( Block (..), BlockHeader (..), BlockHash (..), Nonce (..)
  , ChainStateT (..), BlockMap (..), ChainState (..))

import Control.Monad.Identity (Identity (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (get)
import Data.ByteString as BS (ByteString, take, replicate)
import Data.ByteString.Conversion (toByteString')
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import Data.Serialize (encode)
import Crypto.Hash (hash)

-- TODO: Finish implementing validation.
processBlock :: Block -> ChainStateT Identity ()
processBlock block = do
  state <- get
  case validateBlock block (_mainChain state) (_offChain state) of
    False -> undefined
    True  -> undefined

-- TODO: Add orphan verification stuff.
validateBlock :: Block -> BlockMap -> BlockMap -> Bool
validateBlock block main_chain off_chain =
  checkDuplicate block main_chain off_chain &&
  _transactions block /= mempty &&
  validateBlockHash block &&
  validateTimeStamp block &&
  validateTransactions block &&
  validateMerkleRootHash block &&
  validateDifficulty block


-- | Check if a block is a duplicate of a block we have already seen.
-- TODO: Don't just return False here.
checkDuplicate :: Block -> BlockMap -> BlockMap -> Bool
checkDuplicate block main_chain off_chain = False

validateBlockHash :: Block -> Bool
validateBlockHash = undefined

validateTimeStamp :: Block -> Bool
validateTimeStamp = undefined

validateTransactions :: Block -> Bool
validateTransactions = undefined

validateMerkleRootHash :: Block -> Bool
validateMerkleRootHash = undefined

validateDifficulty :: Block -> Bool
validateDifficulty = undefined


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
blockHashPrefixes block_header target = (fmap . fmap) (BS.take target . encode . hashBlockHeader) . blocks $ block_header

-- | Calculates the proof of work for a block header.
mineBlock ::
  BlockHeader -- ^ The block header without a proof of work.
  -> BlockHeader
mineBlock block_header =
  let target = fromIntegral . _target $ block_header
      prefix    = BS.replicate target 0
      -- Find a nonce with a resulting block header hash prefix that is equal to the string of leading 0's needed to satisfy the target target.
      -- TODO: Using fromJust is probably a bad idea here. Maybe use an error monad or something.
      (nonce, _) = fromJust . find ((==) prefix . snd) . blockHashPrefixes block_header $ target
  in block_header { _nonce = nonce }
