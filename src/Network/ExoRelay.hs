module Network.ExoRelay where

import Data.ByteString as B
import qualified Data.HashMap as HM
import Control.Concurrent.Chan
import Control.Concurrent.MVar

type ListenHandler = B.ByteString -> IO ()

data ExoRelay = ExoRelay {
  port :: Int,
  serviceName :: B.ByteString,
  sendChan :: Chan B.ByteString,
  receiveHandlers :: MVar (HM.Map B.ByteString ListenHandler)
}
