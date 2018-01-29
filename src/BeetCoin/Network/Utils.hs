module BeetCoin.Network.Utils (mkNodeAddress, mkBeetCoinAddress) where

import BeetCoin.Network.Types (NodeAddress (..), BeetCoinAddress (..))

import Control.Distributed.Process (NodeId (..))
import qualified Data.ByteString.Char8 as BS8 (pack)
import Network.Transport (EndPointAddress (..))

mkNodeAddress :: String -> String -> NodeAddress
mkNodeAddress host port = NodeAddress . EndPointAddress . BS8.pack $ host ++ ":" ++ port ++ ":" ++ "0"

mkBeetCoinAddress :: String -> String -> BeetCoinAddress
mkBeetCoinAddress host port =
  BeetCoinAddress . NodeId . EndPointAddress . BS8.pack $ host ++ ":" ++ port ++ ":" ++ "0"