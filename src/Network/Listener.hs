module Network.Listener where

import Network.ExoRelay
import Control.Concurrent.MVar
import System.ZMQ4
import Data.ByteString as B
import qualified Data.HashMap as HM


-- Listener Functions

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
  waitAndRecv exo sock
