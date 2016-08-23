ExoComMock = require 'exocom-mock'

exocom = new ExoComMock


exocom.listen 4100
exocom.registerService {name: 'exorelay-hs', port: 4001 }
