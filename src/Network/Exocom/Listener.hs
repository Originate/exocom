module Network.Exocom.Listener where

import Network.Exocom.ExoRelay
import Network.Exocom.Packet
import Network.Exocom.Sender
import Network.Exocom.Error
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import System.ZMQ4
import qualified Data.HashMap as HM
import Data.Aeson
import Data.Either
import Data.Maybe


-- Listener Functions


listenerThread :: ExoRelay -> Socket Pull -> Int -> IO ()
listenerThread exo sock listenPort = do
  let address = "tcp://*:" ++ (show (listenPort))
  bind sock address
  err <- getError
  case err of
    Nothing -> do
      forkIO $ waitAndRecv exo sock
      return ()
    Just errmsg -> emitError exo errmsg

waitAndRecv :: ExoRelay -> Socket Pull -> IO ()
waitAndRecv exo sock = do
  contents <- receive sock
  err <- getError
  case err of
    Nothing -> do
      let eitherPacket = eitherDecodeStrict contents :: Either String SendPacket
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
    Just errmsg -> emitError exo errmsg
  waitAndRecv exo sock


-- helper function to extract right from an either
extract :: Either a b -> b
extract (Right x) = x
