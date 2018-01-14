module BeetCoin.Blocks
    ( mineBlock
    ) where

import BeetCoin.Types
  ( Block (..), BlockHeader (..), BlockHash (..), Nonce (..)
  , ChainStateT (..), BlockMap (..), ChainState (..), ChainType (..))

import Control.Monad.Identity (Identity (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (get, gets, modify)
import Data.ByteString as BS (ByteString, take, replicate)
import Data.ByteString.Conversion (toByteString')
import Data.List (find)
import qualified Data.Map as HM (insert)
import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import Data.Serialize (encode)
import Crypto.Hash (hash)

-- TODO: Finish implementing validation.
processBlock :: Monad m => Block -> ChainStateT m ()
processBlock block = do
  chain_type <- validateBlock block <$> (gets _mainChain) <*> (gets _sideChain)
  case chain_type of
    NoChain   -> rejectBlock
    MainChain -> modify (addToMainChain block)
    SideChain -> addToSideChain

-- TODO: Add orphan verification stuff.
validateBlock :: Block -> BlockMap -> BlockMap -> ChainType
validateBlock block main_chain side_chain =
  if not (commonBlockValidations block) then
    NoChain
  else if mainChainBlockValidations block then
    MainChain
  else if sideChainBlockValidations block then
    SideChain
  else if orphanChainBlockValidations block then
    NoChain
  else
    NoChain

commonBlockValidations :: Block -> Bool
commonBlockValidations block =
  -- Step 2.
  not (checkDuplicate block) &&
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
  validateDifficulty block

-- | Bunch of validation functions used in commonBlockValidation.
-- TODO: Implement all of these.
checkDuplicate block = False
validateBlockHash block = True
validateTimeStamp block = True
validateTransactions block = True
validateMerkleRootHash block = True
validateDifficulty block = True

-- | Validations for adding a block to a specific chain.
mainChainBlockValidations block = True
sideChainBlockValidations = undefined
orphanChainBlockValidations = undefined

rejectBlock = undefined
addToSideChain = undefined

addToMainChain :: Block -> ChainState -> ChainState
addToMainChain block state = 
  let main_chain = _mainChain state
  in state { _mainChain = addBlock main_chain block }


-- | Add a block to a block map. NOTE: Assumes that the block is not already in the map.
addBlock :: BlockMap -> Block -> BlockMap
addBlock chain block = BlockMap $ HM.insert (hashBlockHeader . _blockHeader $ block) block (_unBlockMap chain)

-- | Hash a block header using sha256.
hashBlockHeader :: BlockHeader -> BlockHash
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
