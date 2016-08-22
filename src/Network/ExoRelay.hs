module Network.ExoRelay where

import Data.ByteString as B
import qualified Data.HashMap as HM
import Control.Concurrent.Chan
import Control.Concurrent.MVar

data MessageHandler = NoReply ListenHandler | Reply ReplyHandler

type ListenHandler = B.ByteString -> IO ()

type ReplyHandler = B.ByteString -> IO (B.ByteString, B.ByteString)

data ExoRelay = ExoRelay {
  port :: Int,
  serviceName :: B.ByteString,
  sendChan :: Chan B.ByteString,
  receiveHandlers :: MVar (HM.Map B.ByteString MessageHandler)
}
