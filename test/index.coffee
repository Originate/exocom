ExoComMock = require 'exocom-mock'

exocom = new ExoComMock


exocom.listen 4100
exocom.registerService {name: 'exorelay-hs', port: 4001 }

exocom.onReceive =>
  console.log 'Received a message'
  console.log exocom.receivedMessages
  toSend = exocom.receivedMessages[0]
  toSend.service = 'exorelay-hs'
  exocom.send toSend
  console.log 'sent message'
