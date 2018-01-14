module BeetCoin.Blocks
    ( mineBlock
    ) where

import BeetCoin.Types
  ( Block (..), BlockHeader (..), BlockHash (..), Nonce (..)
  , ChainStateT (..), BlockMap (..), ChainState (..))

import Control.Monad.Identity (Identity (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (get, gets)
import Data.Bool (bool)
import Data.ByteString as BS (ByteString, take, replicate)
import Data.ByteString.Conversion (toByteString')
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import Data.Serialize (encode)
import Crypto.Hash (hash)

-- TODO: Finish implementing validation.
processBlock :: Monad m => Block -> ChainStateT m ()
processBlock block = do
  is_valid <- validateBlock block <$> (gets _mainChain) <*> (gets _sideChain)
  bool (undefined) (addBlock block) is_valid

-- TODO: Add orphan verification stuff.
validateBlock :: Block -> BlockMap -> BlockMap -> Bool
validateBlock block main_chain side_chain =
  -- Step 2.
  not (checkDuplicate block main_chain side_chain) &&
  -- Step 3.
  _transactions block /= mempty &&
  -- Step 4.
  validateBlockHash block &&
  -- Step 5.
  validateTimeStamp block &&
  -- Step 6-8.
  validateTransactions block &&
  -- Step 9.
  validateMerkleRootHash block &&
  -- Step 10.
  validateDifficulty block &&
  if addToMainChain block main_chain then
    undefined
  else if addToSideChain block side_chain then
    undefined
  else
    undefined

-- | Check if a block is a duplicate of a block we have already seen.
-- TODO: Don't just return False here.
checkDuplicate :: Block -> BlockMap -> BlockMap -> Bool
checkDuplicate block main_chain side_chain = False

validateBlockHash :: Block -> Bool
validateBlockHash block = True

validateTimeStamp :: Block -> Bool
validateTimeStamp block = True

validateTransactions :: Block -> Bool
validateTransactions block = True

validateMerkleRootHash :: Block -> Bool
validateMerkleRootHash block = True

validateDifficulty :: Block -> Bool
validateDifficulty block = True

addToMainChain = undefined
addToSideChain = undefined

addBlock :: Monad m => Block -> ChainStateT m ()
addBlock block = undefined


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
