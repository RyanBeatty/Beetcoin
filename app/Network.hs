module Network where

import qualified Data.ByteString as BS (ByteString (..))
import qualified Data.ByteString.Char8 as BS8 (pack)
import Network.Transport
  ( Transport (..), EndPoint (..), Reliability (..), Connection (..), EndPointAddress (..)
  , Event (..), TransportError (..), ConnectErrorCode (..), defaultConnectHints
  )
import Data.Serialize (Serialize, encode)
import Network.Transport.TCP (createTransport, defaultTCPParameters)

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

mkNodeAddress :: String -> String -> NodeAddress
mkNodeAddress host port = NodeAddress . EndPointAddress . BS8.pack $ host ++ ":" ++ port ++ ":" ++ "0"

mkNode :: Transport -> EndPoint -> Node
mkNode transport endpoint =
  Node (NodeAddress . address $ endpoint)
       (receive endpoint)
       (\address -> connect endpoint address ReliableOrdered defaultConnectHints)
       (closeEndPoint endpoint >> closeTransport transport)

createBeetCoinTransport :: String -> String -> IO (Transport)
createBeetCoinTransport host port = do
  transport <- createTransport host port defaultTCPParameters
  case transport of
    Left _  -> undefined
    Right t -> return t 

createBeetCoinEndPoint :: Transport -> IO (EndPoint)
createBeetCoinEndPoint transport = do
  endpoint <- newEndPoint transport
  case endpoint of
    Left _  -> undefined
    Right e -> return e

makeNode :: String -> String -> IO (Node)
makeNode host port = do
  transport <- createBeetCoinTransport host port
  endpoint <- createBeetCoinEndPoint transport
  return $ mkNode transport endpoint

connectToNode :: Node -> EndPointAddress -> IO (Connection)
connectToNode node peer_address = do
  conn <- _connect node peer_address
  case conn of
    Left _      -> undefined
    Right conn' -> return conn'

sendMessage :: Connection -> BS.ByteString -> IO ()
sendMessage conn msg = do
  result <- send conn [msg]
  case result of
    Left _  -> undefined
    Right _ -> return ()

sendStuff :: Serialize a => Connection -> [a] -> IO ()
sendStuff conn msgs = do
  result <- send conn (encode <$> msgs)
  case result of
    Left _  -> undefined
    Right _ -> return () 
    

setupNetwork :: IO ()
setupNetwork = undefined
