module Main where

import BeetCoin.Core.Blocks (mkChainState, processBlock)
import BeetCoin.Core.Types (ChainStateT (..), ChainState, Block)

import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.State (execState)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString as BS (ByteString, readFile)
import Data.Serialize (decode)
import Data.Traversable (traverse)
import System.IO (FilePath)

getBlocks :: FilePath -> ExceptT String IO [Block]
getBlocks filepath = do
  bytes <- liftIO (BS.readFile filepath)
  ExceptT (return (decode bytes))

initChainState :: [Block] -> ChainState
initChainState blocks =
  let action = _unChainStateT . traverse processBlock $ blocks
  in execState action mkChainState

main :: IO ()
main = do
  let filepath = "beetcoin-blocks"
  putStrLn $ "Initializing ChainState from File: " ++ filepath
  result <- runExceptT (getBlocks filepath >>= return . initChainState)
  case result of
    Left msg    -> putStrLn $ "Failed to Initialize ChainState: " ++ msg
    Right state -> putStrLn $ "Initialized ChainState: " ++ (show state)
