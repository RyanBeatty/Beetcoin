{-# LANGUAGE DeriveGeneric, DeriveFunctor, GeneralizedNewtypeDeriving #-}
module BeetCoin.Network.Types
  ( FooState (..), FooT (..)
  , SendError (..)
  , NodeAddress (..)
  , Message (..)
  , Letter (..)
  , NodeState (..)
  , Node (..)
  ) where

import Control.Monad.State(MonadState, StateT (..))
import Control.Monad.Trans (MonadIO)
import qualified Data.Map.Strict as HM (Map (..))
import Data.Serialize (Serialize (..), put, get)
import GHC.Generics (Generic)
import Network.Transport
  ( Transport (..), EndPoint (..), Reliability (..), Connection (..), EndPointAddress (..)
  , Event (..), TransportError (..), ConnectErrorCode (..), SendErrorCode (..)
  , ConnectionId (..)
  )

hello = undefined

data FooState = FooState { _foo :: HM.Map EndPointAddress Connection }

newtype FooT a = FooT { _fooT :: StateT FooState (IO) a }
  deriving (Functor, Applicative, Monad, MonadState FooState, MonadIO)

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

data NodeState = NodeState
  { _outConns :: HM.Map NodeAddress Connection
  , _inConns  :: HM.Map NodeAddress ConnectionId
  }

data Node = Node
  { _address :: NodeAddress -- ^ The address of this Node.
  , _epoll :: IO (Event) -- ^ Blocking wait for IO events.
  --, _send :: Serialize a => NodeAddress -> [a] -> IO ()
  , _connect :: EndPointAddress -> IO (Either (TransportError ConnectErrorCode) Connection) -- ^ Establish connection to another Node.
  , _closeNode :: IO () -- ^ Shutdown the Node.
  }

instance Show Node where
  show node = "Node: " ++ (show . _address $ node)

instance Serialize Message
instance Serialize Letter

instance Serialize NodeAddress where
  put (NodeAddress address) = put . endPointAddressToByteString $ address
  get = NodeAddress . EndPointAddress <$> get