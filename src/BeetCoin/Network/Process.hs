module BeetCoin.Network.Process where

import BeetCoin.Network.Types (ServerAction (..))
import BeetCoin.Network.Utils (mkNodeId)

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
  ( ProcessId (..), Process (..), NodeId (..), send, register, say
  , getSelfPid, receiveWait, nsendRemote, match
  )
import Control.Distributed.Process.Node (LocalNode, newLocalNode, initRemoteTable, runProcess)
import Network.Transport.TCP (createTransport, defaultTCPParameters)

type ServerProcess a = ServerAction Process a

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg

createNode :: String -> String -> IO (LocalNode)
createNode host port = do
  Right transport <- createTransport host port defaultTCPParameters
  newLocalNode transport initRemoteTable >>= return


process1 = do
  node <- createNode "127.0.0.1" "3939"
  runProcess node $ do
    say "process1 running!"
    pid <- getSelfPid
    register "process1" pid
    say "process1 registered!"
    nsendRemote (mkNodeId "127.0.0.1" "4000") "process2" "hello, world"

process2 = do
  node <- createNode "127.0.0.1" "4000"
  runProcess node $ do
    say "process1 running!"
    pid <- getSelfPid
    register "process2" pid
    say "process1 registered!"
    receiveWait [match logMessage]
