module BeetCoin.Blocks
    ( genesisBlock
    , hashBlockHeader
    ) where

import BeetCoin.Types
  ( Block (..), BlockHeader (..), BlockHash (..), Nonce (..)
  , ChainStateT (..), BlockMap (..), ChainState (..), ChainType (..)
  , UtxoMap (..), MerkleHash (..), Timestamp (..), Transaction (..)
  , TxOut (..), BeetCoinAddress (..), PubKeyHash (..))
import BeetCoin.Utils (sha256)

import Control.Monad.Identity (Identity (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (get, gets, modify)
import Data.ByteString as BS (ByteString, take, replicate, pack)
import Data.ByteString.Conversion (toByteString')
import Data.List (find)
import qualified Data.Map as HM (insert, empty)
import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import Data.Serialize (encode)
import Crypto.Hash (digestFromByteString)

-- | Represents the first block in the mainchain. Mostly just put random values for all the fields right now.
-- TODO: Mine a valid genesis block.
genesisBlock :: Block
genesisBlock = Block
  { _blockHeader = BlockHeader
    { _previousBlockHash = BlockHash . sha256 . BS.replicate 1000 $ 0x00
    , _merkleRootHash    = MerkleHash . sha256 . BS.replicate 1000 $ 0x00
    , _target            = 0
    , _timestamp         = fromRational $ 1.00
    , _nonce             = Nonce 0
    }
  , _coinbase     = CoinbaseTransaction
    { _cbOutputs = TxOut
      { _amount          = 0
      , _receiverAddress = BeetCoinAddress . PubKeyHash . fromJust . digestFromByteString . BS.pack $
        [ 0x04, 0x50, 0x86, 0x3A, 0xD6, 0x4A, 0x87, 0xAE, 0x8A, 0x2F, 0xE8, 0x3C, 0x1A, 0xF1, 0xA8, 0x40
        , 0x3C, 0xB5, 0x3F, 0x53, 0xE4, 0x86, 0xD8, 0x51, 0x1D, 0xAD, 0x8A, 0x04, 0x88, 0x7E, 0x5B, 0x23
        , 0x52, 0x2C, 0xD4, 0x70, 0x24, 0x34, 0x53, 0xA2, 0x99, 0xFA, 0x9E, 0x77, 0x23, 0x77, 0x16, 0x10
        , 0x3A, 0xBC, 0x11, 0xA1, 0xDF, 0x38, 0x85, 0x5E, 0xD6, 0xF2, 0xEE, 0x18, 0x7E, 0x9C, 0x58, 0x2B
        , 0xA6
        ]
      }
    }
  , _transactions = mempty
  }

initChainState :: ChainState
initChainState = ChainState
  { _mainChain = addBlock (BlockMap HM.empty) genesisBlock
  , _sideChain = BlockMap HM.empty
  , _txPool    = UtxoMap HM.empty
  }

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
hashBlockHeader = BlockHash . sha256 . encode

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
