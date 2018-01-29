module BeetCoin.Network.Utils (mkBcNodeId, mkBcNetworkAddress) where

import BeetCoin.Network.Types 
  ( BcNodeId (..), Host, Port, BcProcessName
  , BcNetworkAddress (..)
  )

import Control.Distributed.Process (NodeId (..))
import qualified Data.ByteString.Char8 as BS8 (pack)
import Network.Transport (EndPointAddress (..))

mkBcNodeId :: Host -> Port -> BcNodeId
mkBcNodeId host port =
  BcNodeId . NodeId . EndPointAddress . BS8.pack $ host ++ ":" ++ port ++ ":" ++ "0"

mkBcNetworkAddress :: Host -> Port -> BcProcessName -> BcNetworkAddress
mkBcNetworkAddress host port process_name = BcNetworkAddress (mkBcNodeId host port) process_name