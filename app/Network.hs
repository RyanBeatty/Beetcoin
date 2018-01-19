module Network where

import Network.Transport (Transport (..), EndPoint (..))
import Network.Transport.TCP (createTransport, defaultTCPParameters)

data Node = Node
  { _transport :: Transport
  , _endpoint :: EndPoint
  }

makeBeetCoinTransport :: String -> String -> IO (Transport)
makeBeetCoinTransport host port = do
  transport <- createTransport host port defaultTCPParameters
  case transport of
    Left _  -> undefined
    Right t -> return t 

makeBeetCoinEndPoint :: Transport -> IO (EndPoint)
makeBeetCoinEndPoint transport = do
  endpoint <- newEndPoint transport
  case endpoint of
    Left _  -> undefined
    Right e -> return e

makeNode :: String -> String -> IO (Node)
makeNode host port = do
  transport <- makeBeetCoinTransport host port
  endpoint <- makeBeetCoinEndPoint transport
  return $ Node transport endpoint

setupNetwork :: IO ()
setupNetwork = undefined
