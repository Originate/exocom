{-# LANGUAGE OverloadedStrings #-}
module Network.Exocom where

import System.ZMQ4
import Control.Concurrent.MVar
import Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap as HM
import Control.Concurrent.Chan
import Control.Concurrent
import Network.Packet
import Network.ExoRelay
import Network.Sender
import Network.Listener


newExoRelay :: Int -> B.ByteString -> IO ExoRelay
newExoRelay portNum service = do
  let handlerMap = HM.empty
  handlerMapLock <- newMVar handlerMap -- newMVar :: IO (MVar HashMap)
  sendchan <- newChan
  newContext <- context
  oSock <- socket newContext Push
  iSock <- socket newContext Pull
  let exo = ExoRelay portNum service sendchan handlerMapLock
  _ <- forkIO (senderThread exo oSock)
  _ <- forkIO (listenerThread exo iSock)
  return exo
