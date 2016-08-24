{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Network.Exocom.Packet where


import Data.ByteString as B
import Data.Aeson
import Data.Aeson.Types
import Data.UUID
import Data.UUID.V4
import Data.Maybe
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import GHC.Generics


data SendPacket = SendPacket {
  name :: B.ByteString,
  sender :: Maybe B.ByteString,
  msgId :: B.ByteString,
  payload :: B.ByteString,
  responseTo :: Maybe B.ByteString
} deriving (Generic, Show)

instance ToJSON SendPacket where
  toJSON packet = object keyListFinal where
    keyList = [
      "name" .= (SB.unpack (name packet)),
      "id" .= (SB.unpack (msgId packet)),
      "payload" .= jsonPayload]
    keyList1
      | isJust (responseTo packet) = "response-to" .= (SB.unpack (fromJust (responseTo packet))) : keyList
      | otherwise = keyList
    keyListFinal
      | isJust (sender packet) = "sender" .= (SB.unpack (fromJust (sender packet))) : keyList1
      | otherwise = keyList1
    jsonPayload = (fromJust (decode (LB.fromStrict (payload packet)))) :: Value

instance FromJSON SendPacket where
  parseJSON (Object v) = do
    nameString <- v .: "name"
    senderString <- v .:? "sender"
    msgIdString <- v .: "id"
    payloadString <- v .: "payload" :: Parser (Value)
    responseToString <- v .:? "response-to"
    let nameByteString = SB.pack nameString
    let msgIdUUID = SB.pack msgIdString
    let payloadByteString = LB.toStrict $ encode payloadString
    let responseToUUID = fmap SB.pack responseToString
    let senderMaybe = fmap SB.pack senderString
    return $ SendPacket nameByteString senderMaybe msgIdUUID payloadByteString responseToUUID
