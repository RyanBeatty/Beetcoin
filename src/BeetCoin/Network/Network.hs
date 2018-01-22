module BeetCoin.Network.Network where

import BeetCoin.Network.Types
  ( NodeNetwork (..), NodeAddress (..), Message (..), Letter (..), NetworkState (..)
  , SendError (..), Network (..)
  )

import Control.Monad (forever)
import Control.Monad.RWS (RWST (..), runRWST, asks, ask)
import Control.Monad.State (put, get)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString as BS (ByteString (..))
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Data.Map.Strict as HM (Map (..), lookup, delete, insert, empty)
import Network.Transport
  ( Transport (..), EndPoint (..), Reliability (..), Connection (..), EndPointAddress (..)
  , Event (..), TransportError (..), ConnectErrorCode (..), defaultConnectHints
  )
import Data.Either (rights)
import Data.Serialize (Serialize, encode, decode)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
      
mkNodeAddress :: String -> String -> NodeAddress
mkNodeAddress host port = NodeAddress . EndPointAddress . BS8.pack $ host ++ ":" ++ port ++ ":" ++ "0"

mkNetwork :: Transport -> EndPoint -> Network
mkNetwork transport endpoint =
  Network (NodeAddress . address $ endpoint)
              (receive endpoint)
              (\address -> connect endpoint (_unNodeAddress address) ReliableOrdered defaultConnectHints)
              (\conn letters -> send conn (encode <$> letters))
              (closeEndPoint endpoint >> closeTransport transport)

createNetworkParams :: String -> String -> IO (Network, NetworkState)
createNetworkParams host port = do
  Right transport <- createTransport host port defaultTCPParameters
  Right endpoint  <- newEndPoint transport
  return (mkNetwork transport endpoint, NetworkState HM.empty HM.empty)

-- createNode :: String -> String -> Node ()
-- createNode host port = do
--   transport <- createBeetCoinTransport host port
--   endpoint <- createBeetCoinEndPoint transport
--   mkNode transport endpoint

-- runNode :: Node -> IO ()
-- runNode node = forever $ do
--   letters <- receiveLetters node
--   let responses = handLetters letters
--   sendLetters node responses

-- handLetters = undefined

-- sendLetters = undefined

-- | Send some data. Connects to specified peer if not already connected.
-- TODO: Accumulate connection and send errors in a Writer monad.
sendData :: MonadIO m => NodeAddress -> [Letter] -> NodeNetwork m ()
sendData address letters = do
  network_state <- get
  network <- ask
  let connections = _outConns network_state
  -- Check if we already have a connection to the peer.
  case HM.lookup address connections of
    -- If we aren't connected to the peer, then attempt to establish a new connection.
    Nothing -> do
      new_conn <- liftIO $ (_connect network) address
      case new_conn of
        -- Don't send anything if we can't connect to the peer.
        Left error      -> return ()
        -- Attempt to send the data to the peer.
        Right new_conn' -> do
          result <- liftIO $ (_send network) new_conn' letters
          case result of
            -- Cleanup the connection if something went wrong.
            Left error -> liftIO $ close new_conn'
            -- Add the new connection to our connection map.
            Right ()   -> put $ network_state { _outConns = (HM.insert address new_conn' connections) }
    -- Attempt to send the data if we already have a connection.
    Just conn -> do
      result <- liftIO $ (_send network) conn letters
      case result of
        -- Cleanup the connection and remove it from our connection map if something went wrong.
        Left error -> liftIO (close conn) >> (put $ network_state { _outConns = HM.delete address connections })
        Right ()   -> return ()

-- | Block until some Letters are received by the network.
-- TODO: Implement error case.
receiveData :: MonadIO m => NodeNetwork m [Letter]
receiveData = do
  event <- ask >>= liftIO . _epoll
  case event of
    Received con_id bytes -> return . rights . fmap decode $ bytes
    _                     -> receiveData

    

-- setupSomeNodes :: IO ((Node, Connection), (Node, Connection))
-- setupSomeNodes = do
--   n1 <- createNode "localhost" "3939"
--   n2 <- createNode "localhost" "4000"
--   c1 <- connectToNode n1 (_address . _network $ n2)
--   c2 <- connectToNode n2 (_address . _network $ n1)
--   return ((n1, c1), (n2, c2))

setupNetwork :: IO ()
setupNetwork = undefined
      