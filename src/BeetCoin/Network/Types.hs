module BeetCoin.Network.Types
  ( NodeAddress (..)
  , Node (..)
  ) where

import Network.Transport
  ( Transport (..), EndPoint (..), Reliability (..), Connection (..), EndPointAddress (..)
  , Event (..), TransportError (..), ConnectErrorCode (..)
  )

-- | Unique identifier for a node. Addresses are used to connect to nodes.
-- Takes the form host:port:0.
newtype NodeAddress = NodeAddress { _unNodeAddress :: EndPointAddress }
  deriving (Show, Ord, Eq)

data Node = Node
  { _address :: NodeAddress -- ^ The address of this Node.
  , _epoll :: IO (Event) -- ^ Blocking wait for IO events.
  , _connect :: EndPointAddress -> IO (Either (TransportError ConnectErrorCode) Connection) -- ^ Establish connection to another Node.
  , _closeNode :: IO () -- ^ Shutdown the Node.
  }

instance Show Node where
  show node = "Node: " ++ (show . _address $ node)