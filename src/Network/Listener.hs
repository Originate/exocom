module Network.Listener where

import Network.ExoRelay
import Network.Packet
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import System.ZMQ4
import Data.ByteString as B
import qualified Data.HashMap as HM
import qualified Data.ByteString.Lazy as LB
import Data.Aeson
import Data.Either
import Data.Maybe


-- Listener Functions

-- Takes in an exorelay and a ByteString which corresponds to the command type
-- When a message comes for that message type it will execute the given handler
-- in a separate thread (WARNING: PLEASE DON'T PERFORM ANY NON-THREAD-SAFE OPERATIONS IN THE HANDLER)
registerHandler :: ExoRelay -> B.ByteString -> ListenHandler -> IO ()
registerHandler exo command func = do
  handlers <- takeMVar $ receiveHandlers exo
  let newHandlers = HM.insert command func handlers
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
    when (isJust handlerMaybe) $ forkIO ((fromJust handlerMaybe) (payload packet)) >> return ()
  waitAndRecv exo sock


-- helper function to extract right from an either
extract :: Either a b -> b
extract (Right x) = x
