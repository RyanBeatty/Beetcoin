module BeetCoin.Network.Node where

import BeetCoin.Network.Network
  (createNetwork, runNodeNetwork, mkNetworkState, sendData, receiveData)
import BeetCoin.Network.Types
  ( Node (..), NodeNetwork (..), NodeAddress (..), Letter (..), NodeConfig (..), NodeState (..)
  , Message (..))
import BeetCoin.Network.Utils (mkNodeAddress)

import Control.Monad (forever)
import Control.Monad.RWS (runRWST)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Data.Serialize (encode, decode)

startNode1 :: IO ()
startNode1 = do
  network <- createNetwork "localhost" "3939"
  let peer_address = mkNodeAddress "localhost" "4000"
  let network_action = (runRWST . _unNode) (forever $ connectToNode peer_address) NodeConfig NodeState
  runNodeNetwork network_action network mkNetworkState
  return ()

startNode2 :: IO ()
startNode2 = do
  network <- createNetwork "localhost" "4000"
  let peer_address = mkNodeAddress "localhost" "3939"
  let network_action = (runRWST . _unNode) (forever $ receiveFromNode) NodeConfig NodeState
  runNodeNetwork network_action network mkNetworkState
  return ()

connectToNode :: MonadIO m => NodeAddress -> Node (NodeNetwork m) ()
connectToNode address = lift $ sendData address (pure . encode $ Letter address (mkNodeAddress "localhost" "3939") Message)

receiveFromNode :: MonadIO m => Node (NodeNetwork m) ()
receiveFromNode = do
  letters <- receiveLetters
  lift . liftIO $ print letters

receiveLetters :: MonadIO m => Node (NodeNetwork m) [Letter]
receiveLetters = lift $ receiveData