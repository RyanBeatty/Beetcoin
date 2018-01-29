module BeetCoin.Network.Process where

import BeetCoin.Network.Types
  ( BeetCoinProcess (..), BcNetworkAddress (..), Message (..), Letter (..)
  , ProcessConfig (..), BcNodeId (..)
  )
import BeetCoin.Network.Utils (mkBcNetworkAddress)

import Control.Concurrent (threadDelay)
import Control.Distributed.Process
  ( ProcessId (..), Process (..), NodeId (..), send, register, say
  , getSelfPid, receiveWait, nsendRemote, match
  )
import Control.Distributed.Process.Node (LocalNode, newLocalNode, initRemoteTable, runProcess)
import Control.Monad (forever)
import Control.Monad.RWS (listen, asks)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Network.Transport.TCP (createTransport, defaultTCPParameters)

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg

createNode :: String -> String -> IO (LocalNode)
createNode host port = do
  Right transport <- createTransport host port defaultTCPParameters
  newLocalNode transport initRemoteTable >>= return

-- beetcoinProcess1 :: MonadIO m => BeetCoinProcess m ()
-- beetcoinProcess1 = do
--   sendLetter peer_address HelloMessage


sendLetter :: BcNetworkAddress -> Message -> BeetCoinProcess Process ()
sendLetter peer_address msg = do
  self_address <- asks (_selfAddress)
  lift $ nsendRemote (_unBcNodeId . _nodeId $ peer_address) (_pName peer_address) (Letter self_address peer_address msg)


-- process1 = do
--   node <- createNode "127.0.0.1" "3939"
--   runProcess node $ do
--     say "process1 running!"
--     pid <- getSelfPid
--     register "process1" pid
--     say "process1 registered!"
--     nsendRemote (mkNodeId "127.0.0.1" "4000") "process2" "hello, world"

-- process2 = do
--   node <- createNode "127.0.0.1" "4000"
--   runProcess node $ do
--     say "process1 running!"
--     pid <- getSelfPid
--     register "process2" pid
--     say "process1 registered!"
--     receiveWait [match logMessage]
