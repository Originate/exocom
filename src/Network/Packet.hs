{-# LANGUAGE OverloadedStrings #-}
module Network.Packet where


import Data.ByteString as B
import Data.Aeson
import Data.UUID
import Data.UUID.V4
import Data.Maybe
import qualified Data.ByteString.Char8 as SB


data SendPacket = SendPacket {
  name :: B.ByteString,
  sender :: B.ByteString,
  msgId :: B.ByteString,
  payload :: B.ByteString,
  responseTo :: Maybe B.ByteString
} deriving (Show)

instance ToJSON SendPacket where
  toJSON packet
    | isJust (responseTo packet) =
        object [
          "name" .= (SB.unpack (name packet)),
          "sender" .= (SB.unpack (sender packet)),
          "id" .= (SB.unpack (msgId packet)),
          "payload" .= (SB.unpack (payload packet)),
          "response-to" .= (SB.unpack (fromJust (responseTo packet)))
        ]
    | otherwise =
        object [
          "name" .= (SB.unpack (name packet)),
          "sender" .= (SB.unpack (sender packet)),
          "id" .= (SB.unpack (msgId packet)),
          "payload" .= (SB.unpack (payload packet))
        ]


instance FromJSON SendPacket where
  parseJSON (Object v) = do
    nameString <- v .: "name"
    senderString <- v .: "sender"
    msgIdString <- v .: "id"
    payloadString <- v .: "payload"
    responseToString <- v .:? "response-to"
    let nameByteString = SB.pack nameString
    let senderBytesString = SB.pack senderString
    let msgIdUUID = SB.pack msgIdString
    let payloadByteString = SB.pack payloadString
    let responseToUUID = fmap SB.pack responseToString
    return $ SendPacket nameByteString senderBytesString msgIdUUID payloadByteString responseToUUID

  parseJSON _ = fail "Needs an object"
