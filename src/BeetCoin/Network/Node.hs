module BeetCoin.Network.Node where

import BeetCoin.Network.Types
  ( Node (..), NodeAddress (..), Message (..), Letter (..), NodeState (..)
  , SendError (..), FooT (..), FooState (..)
  )

import Control.Monad (forever)
import Control.Monad.State (put, gets)
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

sendLetters :: Node -> [Letter] -> IO ()
sendLetters node letters = undefined

foo :: Serialize a => NodeAddress -> [a] -> NodeState -> IO (NodeState)
foo address msgs node_state = do
  case HM.lookup address (_outConns node_state) of
    Nothing   -> do
      undefined
    Just conn -> do
      code <- send conn (encode <$> msgs)
      case code of
        Left _   -> return $ node_state { _outConns = HM.delete address (_outConns node_state) }
        Right () -> return node_state 

bar :: Serialize a => EndPoint -> EndPointAddress -> [a] -> HM.Map EndPointAddress Connection -> IO (Either (Either SendError Connection) ())
bar endpoint address msgs connections = do
  case HM.lookup address connections of
    Nothing -> do
      new_conn <- connect endpoint address ReliableOrdered defaultConnectHints
      case new_conn of
        Left error      -> return . Left . Left . ConnectError $ error
        Right new_conn' -> do
          result <- send new_conn' (encode <$> msgs)
          case result of
            Left error -> return . Left . Left . SendError $ error
            Right ()   -> return . Left . Right $ new_conn'
    Just conn -> do
      result <- send conn (encode <$> msgs)
      case result of
        Left error -> return . Left . Left . SendError $ error
        Right ()   -> return . Right $ ()

-- | Send some data. Connects to specified peer if not already connected.
-- TODO: Accumulate connection and send errors in a Writer monad.
--baz :: Serialize a => EndPoint -> EndPointAddress -> [a] -> HM.Map EndPointAddress Connection -> IO (((), HM.Map EndPointAddress Connection))
baz :: Serialize a => EndPoint -> EndPointAddress -> [a] -> FooT ()
baz endpoint address msgs = do
  connections <- gets _foo
  case HM.lookup address connections of
    Nothing -> do
      new_conn <- liftIO $ connect endpoint address ReliableOrdered defaultConnectHints
      case new_conn of
        Left error      -> return ()
        Right new_conn' -> do
          result <- liftIO $ send new_conn' (encode <$> msgs)
          case result of
            Left error -> liftIO $ close new_conn'
            Right ()   -> put $ FooState (HM.insert address new_conn' connections)
    Just conn -> do
      result <- liftIO $ send conn (encode <$> msgs)
      case result of
        Left error -> liftIO (close conn) >> (put . FooState $ HM.delete address connections)
        Right ()   -> return ()

    

setupSomeNodes :: IO ((Node, Connection), (Node, Connection))
setupSomeNodes = do
  n1 <- createNode "localhost" "3939"
  n2 <- createNode "localhost" "4000"
  c1 <- connectToNode n1 (_address n2)
  c2 <- connectToNode n2 (_address n1)
  return ((n1, c1), (n2, c2))

setupNetwork :: IO ()
setupNetwork = undefined
      