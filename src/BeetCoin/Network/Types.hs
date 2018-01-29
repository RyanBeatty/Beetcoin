{-# LANGUAGE DeriveGeneric, DeriveFunctor, GeneralizedNewtypeDeriving #-}
module BeetCoin.Network.Types
  ( SendError (..)
  , NodeAddress (..)
  , BcNodeId (..)
  , Host (..)
  , Port (..)
  , BcProcessName (..)
  , BcNetworkAddress (..)
  , Message (..)
  , Letter (..)
  , Network (..)
  , NetworkState (..)
  , NodeNetwork (..)
  , NodeConfig (..)
  , NodeState (..)
  , Node (..)
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

-- | Unique identifier for a node. Addresses are used to connect to nodes.
-- Takes the form host:port:0.
newtype NodeAddress = NodeAddress { _unNodeAddress :: EndPointAddress }
  deriving (Show, Ord, Eq)

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
  | PeerMessage
  deriving (Show, Generic)

data Letter = Letter
  { _sender   :: BcNetworkAddress
  , _receiver :: BcNetworkAddress
  , _msg      :: Message
  } deriving (Show, Generic)

data SendError =
    SendError (TransportError SendErrorCode)
  | ConnectError (TransportError ConnectErrorCode)
  deriving (Show, Eq)

-- | Interface to the underlying network that the node is running on.
-- TODO: Parameterize with a monad?
data Network = Network
  -- | The addres of this node. Can be used by other nodes to connect to this node.
  { _address      :: NodeAddress
  -- | Blocking wait for IO events
  , _epoll        :: IO (Event)
  -- | Connect to another node.
  , _connect      :: NodeAddress -> IO (Either (TransportError ConnectErrorCode) Connection)
  -- | Send data accross a connection.
  , _send         :: Connection -> [BS.ByteString] -> IO (Either (TransportError SendErrorCode) ())
  -- | Shutdown all network communication.
  , _closeNetwork :: IO ()
  }

-- | Tracks the internal state of connections to and from this Node's network.
data NetworkState = NetworkState
  { _outConns :: HM.Map NodeAddress Connection
  , _inConns  :: HM.Map NodeAddress ConnectionId -- TODO: Do I need this?
  }

-- | Abstraction of the network the Node is running on top of.
newtype NodeNetwork m a = NodeNetwork { _unNodeNetwork :: RWST Network () NetworkState m a }
  deriving (Functor, Applicative, Monad, MonadReader Network, MonadState NetworkState, MonadIO)

data NodeConfig = NodeConfig
  deriving (Show)

data NodeState = NodeState
  deriving (Show)

newtype Node m a = Node { _unNode :: RWST NodeConfig [Letter] NodeState m a }
  deriving ( Functor, Applicative, Monad, MonadReader NodeConfig
           , MonadState NodeState, MonadWriter [Letter], MonadTrans
           )

data ProcessConfig = ProcessConfig { _selfAddress :: BcNetworkAddress }
data ProcessState = ProcessState { _peers :: [BcNetworkAddress] }

newtype BeetCoinProcess m a = BeetCoinProcess { _unBeetCoinProcess :: RWST ProcessConfig [Letter] ProcessState m a }
  deriving ( Functor, Applicative, Monad, MonadReader ProcessConfig, MonadWriter [Letter]
           , MonadState ProcessState, MonadTrans
           )

instance Binary BcNodeId
instance Binary BcNetworkAddress

instance Binary Message
instance Binary Letter

instance Serialize NodeAddress where
  put (NodeAddress address) = put . endPointAddressToByteString $ address
  get = NodeAddress . EndPointAddress <$> get