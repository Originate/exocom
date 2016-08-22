module Network.Listener where

import Network.ExoRelay
import Network.Packet
import Network.Sender
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import System.ZMQ4
import Data.ByteString as B
import qualified Data.HashMap as HM
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as SB
import Data.UUID
import Data.UUID.V4
import Data.Aeson
import Data.Either
import Data.Maybe


-- Listener Functions

-- Takes in an exorelay and a ByteString which corresponds to the command type
-- When a message comes for that message type it will execute the given handler
-- in a separate thread (WARNING: PLEASE DON'T PERFORM ANY NON-THREAD-SAFE OPERATIONS IN THE HANDLER)
registerHandler :: ExoRelay -> B.ByteString -> (B.ByteString -> IO ()) -> IO ()
registerHandler exo command func = do
  handlers <- takeMVar $ receiveHandlers exo
  let newHandlers = HM.insert command (NoReply func) handlers
  putMVar (receiveHandlers exo) newHandlers

registerHandlerWithReply :: ExoRelay -> B.ByteString -> (B.ByteString -> IO (B.ByteString, B.ByteString)) -> IO ()
registerHandlerWithReply exo command func = do
  handlers <- takeMVar $ receiveHandlers exo
  let newHandlers = HM.insert command (Reply func) handlers
  putMVar (receiveHandlers exo) newHandlers


unregisterHandler :: ExoRelay -> B.ByteString -> IO ()
unregisterHandler exo command = do
  handlers <- takeMVar $ receiveHandlers exo
  let newHandlers = HM.delete command handlers
  putMVar (receiveHandlers exo) newHandlers


listenerThread :: ExoRelay -> Socket Pull -> IO ()
listenerThread exo sock = do
  let address = "tcp://*:" ++ (show (port exo))
  bind sock address

waitAndRecv :: ExoRelay -> Socket Pull -> IO ()
waitAndRecv exo sock = do
  contents <- receive sock
  let eitherPacket = eitherDecode (LB.fromStrict contents) :: Either String SendPacket
  when (isRight eitherPacket) $ do
    let packet = extract eitherPacket
    handlers <- readMVar (receiveHandlers exo)
    let handlerMaybe = HM.lookup (name packet) handlers
    when (isJust handlerMaybe) $ do
      let handler = fromJust handlerMaybe
      case handler of
        NoReply hand -> forkIO $ hand (payload packet)
        Reply hand -> forkIO $ do
          unregisterHandler exo (name packet)
          (cmd, repl) <- hand (payload packet)
          if isJust (responseTo packet) then
            sendMsgReply exo cmd repl (fromJust (responseTo packet))
          else
            return ()
      return ()
  waitAndRecv exo sock


-- helper function to extract right from an either
extract :: Either a b -> b
extract (Right x) = x

sendMsgWithReply :: ExoRelay -> B.ByteString -> B.ByteString -> (B.ByteString -> IO ()) -> IO ()
sendMsgWithReply exo cmd payload hand = do
  identUUID <- nextRandom
  let ident = SB.pack $ toString identUUID
  let packet = SendPacket cmd (serviceName exo) ident payload Nothing
  let jsonByteString = encode packet
  registerHandler exo ident (\response -> unregisterHandler exo ident >> hand response)
  writeChan (sendChan exo) (LB.toStrict jsonByteString)
