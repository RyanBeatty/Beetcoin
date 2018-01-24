module BeetCoin.Network.Utils (mkNodeAddress) where

import BeetCoin.Network.Types (NodeAddress (..))

import qualified Data.ByteString.Char8 as BS8 (pack)
import Network.Transport (EndPointAddress (..))

mkNodeAddress :: String -> String -> NodeAddress
mkNodeAddress host port = NodeAddress . EndPointAddress . BS8.pack $ host ++ ":" ++ port ++ ":" ++ "0"