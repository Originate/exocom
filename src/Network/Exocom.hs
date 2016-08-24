{-# LANGUAGE OverloadedStrings #-}
module Network.Exocom (
  ExoRelay,
  newExoRelay,
  sendMsg,
  sendMsgWithReply,
  registerHandler,
  registerHandlerWithReply,
)
where

import System.ZMQ4
import Control.Concurrent.MVar
import Data.ByteString as B
import qualified Data.ByteString.Char8 as SB
import qualified Data.HashMap as HM
import Control.Concurrent.Chan
import Control.Concurrent
import Network.Exocom.Packet
import Network.Exocom.ExoRelay
import Network.Exocom.Sender
import Network.Exocom.Listener
import Network.Exocom.Error
import Data.Maybe


newExoRelay :: Int -> B.ByteString -> Int -> Maybe (String -> IO ()) -> IO ExoRelay
newExoRelay portNum service listenerPort errHandler = do
  resetError
  let handlerMap = HM.empty
  handlerMapLock <- newMVar handlerMap -- newMVar :: IO (MVar HashMap)
  sendchan <- newChan
  errChan <- newChan
  newContext <- context
  oSock <- socket newContext Push
  iSock <- socket newContext Pull
  let exo = ExoRelay portNum service sendchan errChan handlerMapLock errHandler
  let statusCheck = SB.pack "__status"
  registerHandlerWithReply exo statusCheck statusHandler
  errorThread exo
  sendErr <- senderThread exo oSock
  listenErr <- listenerThread exo iSock listenerPort
  return exo

statusHandler :: B.ByteString -> IO (B.ByteString, B.ByteString)
statusHandler _ = Prelude.putStrLn "sending OK" >> return (cmd, emptyByteStr) where
  cmd = SB.pack "__status-ok"
  emptyByteStr = B.empty
