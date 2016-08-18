{-# LANGUAGE OverloadedStrings #-}
module Network.Exocom where

import System.ZMQ4
import Control.Concurrent.MVar
import Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap as HM
import Control.Concurrent.Chan
import Control.Concurrent
import Data.UUID
import Data.UUID.V4
import Data.Aeson
import Data.Maybe


type ListenHandler = B.ByteString -> IO ()

data ExoRelay = ExoRelay {
  port :: Int,
  serviceName :: B.ByteString,
  sendChan :: Chan B.ByteString,
  recieveHandlers :: MVar (HM.Map String ListenHandler)
}

newExoRelay :: Int -> B.ByteString -> IO ExoRelay
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
  responseTo :: Maybe UUID
}

instance ToJSON SendPacket where
  toJSON packet
    | isJust (responseTo packet) =
        object [
          "name" .= (unpack (name packet)),
          "sender" .= (unpack (sender packet)),
          "id" .= (toString (msgId packet)),
          "payload" .= (unpack (payload packet)),
          "response-to" .= (toString (fromJust (responseTo packet)))
        ]
    | otherwise =
        object [
          "name" .= (unpack (name packet)),
          "sender" .= (unpack (sender packet)),
          "id" .= (toString (msgId packet)),
          "payload" .= (unpack (payload packet))
        ]

sendMsgGeneral :: ExoRelay -> B.ByteString -> B.ByteString -> Maybe UUID -> IO ()
sendMsgGeneral exo command toSend respond = do
  ident <- nextRandom
  let packet = SendPacket command (serviceName exo) ident toSend respond
  let jsonByteString = encode packet
  writeChan (sendChan exo) (LB.toStrict jsonByteString)


sendMsg :: ExoRelay -> B.ByteString -> B.ByteString -> IO ()
sendMsg exo command toSend = sendMsgGeneral exo command toSend Nothing
