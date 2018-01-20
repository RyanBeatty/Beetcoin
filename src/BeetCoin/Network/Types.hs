{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module BeetCoin.Network.Types
  ( NodeAddress (..)
  , Message (..)
  , NodeState (..)
  , Node (..)
  ) where

import Control.Monad.State.Class (MonadState)
import Data.Serialize (Serialize)
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

data SendError =
    SendError (TransportError SendErrorCode)
  | ConnectError (TransportError ConnectErrorCode)
  deriving (Show, Eq)

data NodeState = NodeState
  { _outConns :: [(NodeAddress, Connection)]
  , _inConns  :: [(NodeAddress, ConnectionId)]
  }

data Node = Node
  { _address :: NodeAddress -- ^ The address of this Node.
  , _epoll :: IO (Event) -- ^ Blocking wait for IO events.
  -- , _send :: Serialize a => NodeAddress -> [a] -> IO (Either SendError ())
  , _connect :: EndPointAddress -> IO (Either (TransportError ConnectErrorCode) Connection) -- ^ Establish connection to another Node.
  , _closeNode :: IO () -- ^ Shutdown the Node.
  }

instance Show Node where
  show node = "Node: " ++ (show . _address $ node)

instance Serialize Message