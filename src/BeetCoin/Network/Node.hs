module BeetCoin.Network.Node where

import BeetCoin.Network.Types (Node (..), NodeAddress (..), Message (..), Letter (..))

import Control.Monad (forever)
import qualified Data.ByteString as BS (ByteString (..))
import qualified Data.ByteString.Char8 as BS8 (pack)
import Network.Transport
  ( Transport (..), EndPoint (..), Reliability (..), Connection (..), EndPointAddress (..)
  , Event (..), TransportError (..), ConnectErrorCode (..), defaultConnectHints
  )
import Data.Either (rights)
import Data.Serialize (Serialize, encode, decode)
import Network.Transport.TCP (createTransport, defaultTCPParameters)      
      
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

createNode :: String -> String -> IO (Node)
createNode host port = do
  transport <- createBeetCoinTransport host port
  endpoint <- createBeetCoinEndPoint transport
  return $ mkNode transport endpoint

connectToNode :: Node -> NodeAddress -> IO (Connection)
connectToNode node peer_address = do
  conn <- _connect node (_unNodeAddress peer_address)
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

runNode :: Node -> IO ()
runNode node = forever $ do
  letters <- receiveLetters node
  let responses = handLetters letters
  sendLetters node responses

-- NOTE: This only works once a connection has been established to another node.
receiveLetters :: Node -> IO ([Letter])
receiveLetters node = do
  Received conn_id raw_msgs <- _epoll node
  return . rights $ (decode <$> raw_msgs)

handLetters = undefined

sendLetters :: Node -> [Letters] -> IO ()
sendLetters node letters = undefined

    

setupSomeNodes :: IO ((Node, Connection), (Node, Connection))
setupSomeNodes = do
  n1 <- createNode "localhost" "3939"
  n2 <- createNode "localhost" "4000"
  c1 <- connectToNode n1 (_address n2)
  c2 <- connectToNode n2 (_address n1)
  return ((n1, c1), (n2, c2))

setupNetwork :: IO ()
setupNetwork = undefined
      