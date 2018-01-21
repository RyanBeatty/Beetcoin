module BeetCoin.Network.Node where

import BeetCoin.Network.Types
  ( Node (..), NodeAddress (..), Message (..), Letter (..), NodeState (..)
  , SendError (..), FooT (..), NodeNetwork (..)
  )

import Control.Monad (forever)
import Control.Monad.State (put, get)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString as BS (ByteString (..))
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Data.Map.Strict as HM (Map (..), lookup, delete, insert)
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
  Node $ NodeNetwork (NodeAddress . address $ endpoint)
                     (receive endpoint)
                     (\address -> connect endpoint address ReliableOrdered defaultConnectHints)
                     (\conn letters -> send conn (encode <$> letters))
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
  conn <- _connect (_network node) (_unNodeAddress peer_address)
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
  Received conn_id raw_msgs <- _epoll (_network node)
  return . rights $ (decode <$> raw_msgs)

handLetters = undefined

sendLetters :: Node -> [Letter] -> IO ()
sendLetters node letters = undefined

-- | Send some data. Connects to specified peer if not already connected.
-- TODO: Accumulate connection and send errors in a Writer monad.
sendData :: Serialize a => EndPoint -> NodeAddress -> [a] -> FooT ()
sendData endpoint address msgs = do
  node_state <- get
  let connections = _outConns node_state
  -- Check if we already have a connection to the peer.
  case HM.lookup address connections of
    -- If we aren't connected to the peer, then attempt to establish a new connection.
    Nothing -> do
      new_conn <- liftIO $ connect endpoint (_unNodeAddress address) ReliableOrdered defaultConnectHints
      case new_conn of
        -- Don't send anything if we can't connect to the peer.
        Left error      -> return ()
        -- Attempt to send the data to the peer.
        Right new_conn' -> do
          result <- liftIO $ send new_conn' (encode <$> msgs)
          case result of
            -- Cleanup the connection if something went wrong.
            Left error -> liftIO $ close new_conn'
            -- Add the new connection to our connection map.
            Right ()   -> put $ node_state { _outConns = (HM.insert address new_conn' connections) }
    -- Attempt to send the data if we already have a connection.
    Just conn -> do
      result <- liftIO $ send conn (encode <$> msgs)
      case result of
        -- Cleanup the connection and remove it from our connection map if something went wrong.
        Left error -> liftIO (close conn) >> (put $ node_state { _outConns = HM.delete address connections })
        Right ()   -> return ()

    

setupSomeNodes :: IO ((Node, Connection), (Node, Connection))
setupSomeNodes = do
  n1 <- createNode "localhost" "3939"
  n2 <- createNode "localhost" "4000"
  c1 <- connectToNode n1 (_address . _network $ n2)
  c2 <- connectToNode n2 (_address . _network $ n1)
  return ((n1, c1), (n2, c2))

setupNetwork :: IO ()
setupNetwork = undefined
      