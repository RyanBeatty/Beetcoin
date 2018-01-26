module BeetCoin.Network.Utils (mkNodeAddress, mkNodeId) where

import BeetCoin.Network.Types (NodeAddress (..))

import Control.Distributed.Process (NodeId (..))
import qualified Data.ByteString.Char8 as BS8 (pack)
import Network.Transport (EndPointAddress (..))

mkNodeAddress :: String -> String -> NodeAddress
mkNodeAddress host port = NodeAddress . EndPointAddress . BS8.pack $ host ++ ":" ++ port ++ ":" ++ "0"

mkNodeId :: String -> String -> NodeId
mkNodeId host port = NodeId . EndPointAddress . BS8.pack $ host ++ ":" ++ port ++ ":" ++ "0"