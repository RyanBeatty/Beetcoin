module Network where

import qualified Data.ByteString as BS (ByteString (..))
import qualified Data.ByteString.Char8 as BS8 (pack)
import Network.Transport
  ( Transport (..), EndPoint (..), Reliability (..), Connection (..), EndPointAddress (..)
  , defaultConnectHints
  )
import Network.Transport.TCP (createTransport, defaultTCPParameters)

data Node = Node
  { _transport :: Transport
  , _endpoint :: EndPoint
  , _closeNode :: IO ()
  }

instance Show Node where
  show (Node _ endpoint _) = "Node: " ++ (show . address $ endpoint)

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
  return $ Node transport endpoint (closeEndPoint endpoint >> closeTransport transport)

connectToNode :: Node -> EndPointAddress -> IO (Connection)
connectToNode node peer_address = do
  conn <- connect (_endpoint node) peer_address ReliableOrdered defaultConnectHints
  case conn of
    Left _      -> undefined
    Right conn' -> return conn'

sendMessage :: Connection -> BS.ByteString -> IO ()
sendMessage conn msg = do
  result <- send conn [msg]
  case result of
    Left _  -> undefined
    Right _ -> return ()
    

setupNetwork :: IO ()
setupNetwork = undefined
