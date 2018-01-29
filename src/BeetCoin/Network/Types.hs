{-# LANGUAGE DeriveGeneric, DeriveFunctor, GeneralizedNewtypeDeriving #-}
module BeetCoin.Network.Types
  ( BcNodeId (..)
  , Host (..)
  , Port (..)
  , BcProcessName (..)
  , BcNetworkAddress (..)
  , Message (..)
  , Letter (..)
  , ProcessConfig (..)
  , ProcessState (..)
  , BeetCoinProcess (..)
  ) where

import Control.Distributed.Process (NodeId (..))
import Control.Monad.RWS (RWST (..), MonadReader, MonadWriter)
import Control.Monad.State (MonadState, StateT (..))
import Control.Monad.Trans (MonadTrans, MonadIO)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Map.Strict as HM (Map (..))
import Data.Binary (Binary)
import Data.Serialize (Serialize (..), put, get)
import GHC.Generics (Generic)
import Network.Transport
  ( Transport (..), EndPoint (..), Reliability (..), Connection (..), EndPointAddress (..)
  , Event (..), TransportError (..), ConnectErrorCode (..), SendErrorCode (..)
  , ConnectionId (..)
  )

newtype BcNodeId = BcNodeId { _unBcNodeId :: NodeId }
  deriving (Show, Generic)

type Host          = String
type Port          = String
type BcProcessName = String

data BcNetworkAddress = BcNetworkAddress
    { _nodeId :: BcNodeId
    , _pName  :: BcProcessName
    } deriving (Show, Generic)

data Message =
    HelloMessage  
  | PeerMessage { _peers :: [BcNetworkAddress] }
  deriving (Show, Generic)

data Letter = Letter
  { _sender   :: BcNetworkAddress
  , _receiver :: BcNetworkAddress
  , _msg      :: Message
  } deriving (Show, Generic)

data ProcessConfig = ProcessConfig { _selfAddress :: BcNetworkAddress }
data ProcessState = ProcessState { _myPeers :: [BcNetworkAddress] }

newtype BeetCoinProcess m a = BeetCoinProcess { _unBeetCoinProcess :: RWST ProcessConfig [Letter] ProcessState m a }
  deriving ( Functor, Applicative, Monad, MonadReader ProcessConfig, MonadWriter [Letter]
           , MonadState ProcessState, MonadTrans
           )

instance Binary BcNodeId
instance Binary BcNetworkAddress

instance Binary Message
instance Binary Letter