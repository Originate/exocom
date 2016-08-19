module Network.Sender where

import System.ZMQ4
import Network.ExoRelay
import Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Control.Concurrent.Chan
import Data.UUID
import Data.UUID.V4
import Data.Aeson
import Network.Packet



-- Sender Functions


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


-- internal sending of
sendMsgGeneral :: ExoRelay -> B.ByteString -> B.ByteString -> Maybe UUID -> IO ()
sendMsgGeneral exo command toSend respond = do
  ident <- nextRandom
  let packet = SendPacket command (serviceName exo) ident toSend respond
  let jsonByteString = encode packet
  writeChan (sendChan exo) (LB.toStrict jsonByteString)


-- sendMsg takes in the exorelay object, a command type and a payload and sends it
sendMsg :: ExoRelay -> B.ByteString -> B.ByteString -> IO ()
sendMsg exo command toSend = sendMsgGeneral exo command toSend Nothing
