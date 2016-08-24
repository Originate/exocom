module Network.Exocom.Error (
  getError
)
where

import Foreign.C.Error
import Foreign.C.Types
import System.ZMQ4.Internal.Error

-- getError retrieves a ZMQ error if any and resets errno
getError :: IO (Maybe String)
getError = do
  err <- getErrno
  if err == eOK then return Nothing
  else do
    msg <- zmqErrnoMessage (extract err)
    resetErrno
    return $ Just msg


extract :: Errno -> CInt
extract (Errno val) = val
