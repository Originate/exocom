module Main where

import Network.Exocom
import Control.Concurrent
import Control.Concurrent.MVar
import System.Process
import System.Exit
import System.IO
import Data.Aeson
import Data.ByteString as B hiding (putStrLn)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB


echoHandler :: MVar Value -> Value -> IO ()
echoHandler = putMVar

main :: IO ()
main = do
  exo <- newExoRelay 4100 "exorelay-hs" 4001 (Just handleError)
  didRoundTrip <- roundTrip exo
  if didRoundTrip then exitSuccess else exitFailure


roundTrip :: ExoRelay -> IO Bool
roundTrip exo = do
  ctrlVar <- newEmptyMVar
  registerHandler exo "hello" (echoHandler ctrlVar)
  sendMsg exo "hello" (toJSON "payload")
  res <- readMVar ctrlVar
  return $ res == (toJSON "payload") where



handleError :: String -> IO ()
handleError str = do
  Prelude.putStrLn $ "Error found: " ++ str
  hFlush stdout
