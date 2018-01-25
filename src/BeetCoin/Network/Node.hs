module BeetCoin.Network.Node where

import BeetCoin.Network.Network
  (createNetwork, runNodeNetwork, mkNetworkState, sendData, receiveData)
import BeetCoin.Network.Types
  ( Node (..), NodeNetwork (..), NodeAddress (..), Letter (..), NodeConfig (..), NodeState (..)
  , Message (..))
import BeetCoin.Network.Utils (mkNodeAddress)

import Control.Monad (forever)
import Control.Monad.RWS (runRWST, tell, listen)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Data.Serialize (encode, decode)
import Data.Traversable (traverse)

startNode1 :: IO ()
startNode1 = do
  network <- createNetwork "localhost" "3939"
  let my_address     = mkNodeAddress "localhost" "3939" 
  let peer_address   = mkNodeAddress "localhost" "4000"
  let network_action = runNode (forever $ sendLetter (mkLetter my_address peer_address Message)) NodeConfig NodeState
  runNodeNetwork network_action network mkNetworkState
  return ()

startNode2 :: IO ()
startNode2 = do
  network <- createNetwork "localhost" "4000"
  let peer_address = mkNodeAddress "localhost" "3939"
  let network_action = runNode (forever $ receiveFromNode) NodeConfig NodeState
  runNodeNetwork network_action network mkNetworkState
  return ()

runNode :: MonadIO m => Node (NodeNetwork m) a -> NodeConfig -> NodeState -> NodeNetwork m (a, NodeState, [Letter])
runNode = runRWST . _unNode 

mkLetter :: NodeAddress -> NodeAddress -> Message -> Letter
mkLetter sender receiver msg = Letter sender receiver msg

sendLetter :: MonadIO m => Letter -> Node (NodeNetwork m) ()
sendLetter letter = lift $ sendData (_receiver letter) (pure . encode $ letter)

receiveFromNode :: MonadIO m => Node (NodeNetwork m) ()
receiveFromNode = do
  letters <- receiveLetters
  lift . liftIO $ print letters

receiveLetters :: MonadIO m => Node (NodeNetwork m) [Letter]
receiveLetters = lift $ receiveData

startEventLoop :: MonadIO m => Node (NodeNetwork m) ()
startEventLoop = do
  actions         <- waitForLetters handler
  (_, letters_to_send) <- listen actions
  traverse (sendLetter) letters_to_send
  startEventLoop

handler = undefined
waitForLetters = undefined