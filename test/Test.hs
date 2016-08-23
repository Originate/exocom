module Main where

import Network.Exocom
import Control.Concurrent
import Control.Concurrent.MVar
import System.Process
import System.Exit
import Data.Aeson
import Data.ByteString as B hiding (putStrLn)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB


echoHandler :: MVar B.ByteString -> B.ByteString -> IO ()
echoHandler = putMVar

main :: IO ()
main = do
  exo <- newExoRelay 4100 (SB.pack "exorelay-hs") 4001
  didRoundTrip <- roundTrip exo
  if didRoundTrip then exitSuccess else exitFailure


roundTrip :: ExoRelay -> IO Bool
roundTrip exo = do
  ctrlVar <- newEmptyMVar
  registerHandler exo packedHello (echoHandler ctrlVar)
  sendMsg exo packedHello "payload"
  res <- readMVar ctrlVar
  return $ res == (LB.toStrict (encode "payload")) where
    packedHello = SB.pack "hello"
