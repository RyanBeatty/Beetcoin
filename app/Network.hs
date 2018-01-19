module Network where

import qualified Data.ByteString as BS (ByteString (..))
import qualified Data.ByteString.Char8 as BS8 (pack)
import Network.Transport
  ( Transport (..), EndPoint (..), Reliability (..), Connection (..), EndPointAddress (..)
  , Event (..), TransportError (..), ConnectErrorCode (..), defaultConnectHints
  )
import Data.Serialize (Serialize, encode)
import Network.Transport.TCP (createTransport, defaultTCPParameters)

data Node = Node
  { _address :: EndPointAddress -- ^ The address of this Node.
  , _epoll :: IO (Event) -- ^ Blocking wait for IO events.
  , _connect :: EndPointAddress -> IO (Either (TransportError ConnectErrorCode) Connection) -- ^ Establish connection to another Node.
  , _closeNode :: IO () -- ^ Shutdown the Node.
  }

instance Show Node where
  show node = "Node: " ++ (show . _address $ node)

makeEndPointAddress :: String -> String -> EndPointAddress
makeEndPointAddress host port = EndPointAddress . BS8.pack $ host ++ ":" ++ port ++ ":" ++ "0"

makeBeetCoinTransport :: String -> String -> IO (Transport)
makeBeetCoinTransport host port = do
  transport <- createTransport host port defaultTCPParameters
  case transport of
    Left _  -> undefined
    Right t -> return t 

makeBeetCoinEndPoint :: Transport -> IO (EndPoint)
makeBeetCoinEndPoint transport = do
  endpoint <- newEndPoint transport
  case endpoint of
    Left _  -> undefined
    Right e -> return e

makeNode :: String -> String -> IO (Node)
makeNode host port = do
  transport <- makeBeetCoinTransport host port
  endpoint <- makeBeetCoinEndPoint transport
  return $ Node (address endpoint)
                (receive endpoint)
                (\address -> connect endpoint address ReliableOrdered defaultConnectHints)
                (closeEndPoint endpoint >> closeTransport transport)

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
