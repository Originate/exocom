module Network.Exocom.Listener where

import Network.Exocom.ExoRelay
import Network.Exocom.Packet
import Network.Exocom.Sender
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


listenerThread :: ExoRelay -> Socket Pull -> Int -> IO ()
listenerThread exo sock listenPort = do
  let address = "tcp://*:" ++ (show (listenPort))
  bind sock address
  waitAndRecv exo sock

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
