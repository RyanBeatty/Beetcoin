module BeetCoin.Network.Node where

import BeetCoin.Network.Network (createNetwork, runNodeNetwork, mkNetworkState)
import BeetCoin.Network.Types (Node (..), NodeNetwork (..))

import Control.Monad.RWS (runRWST)
import Control.Monad.Trans (MonadIO, liftIO)

foo :: IO ()
foo = do
  network <- createNetwork "localhost" "3939"
  let network_action = (runRWST . _unNode) undefined undefined undefined
  runNodeNetwork network_action network mkNetworkState
  return ()
