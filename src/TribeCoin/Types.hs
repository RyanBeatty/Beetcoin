{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TribeCoin.Types
    ( BlockHash (..)
    , Timestamp (..)
    , Difficulty (..)
    , Nonce (..)
    , BlockHeader (..)
    , Block (..)
    ) where

import Crypto.Hash (Digest, SHA256)
import Data.Word (Word32, Word8)

newtype BlockHash = BlockHash (Digest SHA256)
      deriving (Show, Eq)

newtype Timestamp = Timestamp Word32
      deriving (Show, Eq, Ord)

newtype Difficulty = Difficulty Word32
      deriving (Show)

newtype Nonce = Nonce Word32
      deriving (Show, Enum)

data BlockHeader = BlockHeader
  { _previousBlockHash :: BlockHash
  , _timestamp :: Timestamp
  , _difficulty :: Difficulty
  , _nonce :: Nonce
  } deriving (Show)

data Input = Input 
  { _previousTrans :: ()
  , _transIndex :: ()
  , _signature :: ()
  } deriving (Show)

newtype Amount = Amount Word8
      deriving (Show, Eq)

data Output = Output
  { _amount :: Amount
  , _address :: ()
  } deriving (Show)

data Transaction = Transaction
  { _inputs :: [Input]
  , _outpus :: [Output]
  } deriving (Show)

data Block = Block
  { _blockHeader :: BlockHeader
  , _transactions :: [Transaction]
  } deriving (Show)

