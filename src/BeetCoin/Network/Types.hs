{-# LANGUAGE DeriveGeneric, DeriveFunctor, GeneralizedNewtypeDeriving #-}
module BeetCoin.Network.Types
  ( SendError (..)
  , NodeAddress (..)
  , Message (..)
  , Letter (..)
  , Network (..)
  , NetworkState (..)
  , NodeNetwork (..)
  ) where

import Control.Monad.RWS (RWST (..), MonadReader)
import Control.Monad.State (MonadState, StateT (..))
import Control.Monad.Trans (MonadTrans, MonadIO)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Map.Strict as HM (Map (..))
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

data Message = Message
  deriving (Show, Generic)

data Letter = Letter
  { _sender   :: NodeAddress
  , _receiver :: NodeAddress
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

data NetworkState = NetworkState
  { _outConns :: HM.Map NodeAddress Connection
  , _inConns  :: HM.Map NodeAddress ConnectionId
  }

newtype NodeNetwork m a = NodeNetwork { _unNodeNetwork :: RWST Network () NetworkState m a }
  deriving (Functor, Applicative, Monad, MonadReader Network, MonadState NetworkState, MonadIO)

newtype NodeAction m a = NodeAction { _unNodeAction :: RWST () () () m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

type Node m a = NodeAction (NodeNetwork m) a

instance Serialize Message
instance Serialize Letter

instance Serialize NodeAddress where
  put (NodeAddress address) = put . endPointAddressToByteString $ address
  get = NodeAddress . EndPointAddress <$> get