module BeetCoin.Network.Process where

import BeetCoin.Network.Types
import BeetCoin.Network.Network
import BeetCoin.Network.Utils

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg

mkNodeId :: String -> String -> NodeId
mkNodeId host port = NodeId . _unNodeAddress $ mkNodeAddress host port


process1 = do
  Right t <- createTransport "127.0.0.1" "3939" defaultTCPParameters
  node    <- newLocalNode t initRemoteTable
  runProcess node $ do
    say "process1 running!"
    pid <- getSelfPid
    register "process1" pid
    say "process1 registered!"
    nsendRemote (mkNodeId "127.0.0.1" "4000") "process2" "hello, world"

process2 = do
  Right t <- createTransport "127.0.0.1" "4000" defaultTCPParameters
  node    <- newLocalNode t initRemoteTable
  runProcess node $ do
    say "process1 running!"
    pid <- getSelfPid
    register "process2" pid
    say "process1 registered!"
    receiveWait [match logMessage]