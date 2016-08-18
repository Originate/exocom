{-# LANGUAGE OverloadedStrings #-}
module Network.Exocom where

import System.ZMQ4
import Control.Concurrent.MVar
import Data.ByteString as B
import Data.HashMap as HM
import Control.Concurrent.Chan
import Control.Concurrent
import Data.UUID
import Data.Aeson


type ListenHandler = B.ByteString -> IO ()

data ExoRelay = ExoRelay {
  port :: Int,
  serviceName :: String,
  sendChan :: Chan B.ByteString,
  recieveHandlers :: MVar (HM.Map String ListenHandler)
}

newExoRelay :: Int -> String -> IO ExoRelay
newExoRelay portNum service = do
  let handlerMap = HM.empty
  handlerMapLock <- newMVar handlerMap
  sendchan <- newChan
  newContext <- context
  oSock <- socket newContext Push
  iSock <- socket newContext Pull
  let exo = ExoRelay portNum service sendchan handlerMapLock
  _ <- forkIO (senderThread exo oSock)
  return exo


senderThread :: ExoRelay -> Socket Push -> IO ()
senderThread exo sock = do
  let address = "tcp://localhost:" ++ (show (port exo))
  connect sock address
  waitAndSend exo sock

waitAndSend :: ExoRelay -> Socket Push -> IO ()
waitAndSend exo sock = do
  toSend <- readChan $ sendChan exo
  send sock [] toSend
  waitAndSend exo sock


data SendPacket = SendPacket {
  name :: B.ByteString,
  sender :: B.ByteString,
  msgId :: UUID,
  payload :: B.ByteString,
  responseTo :: UUID
}

instance ToJSON SendPacket where
  toJSON packet =
    object [
      "name" .= (unpack (name packet)),
      "sender" .= (unpack (sender packet)),
      "id" .= (toString (msgId packet)),
      "payload" .= (unpack (payload packet)),
      "response-to" .= (toString (responseTo packet))
    ]

sendMsg :: ExoRelay -> B.ByteString -> B.ByteString -> IO ()
sendMsg exo command toSend = writeChan (sendChan exo) toSend
