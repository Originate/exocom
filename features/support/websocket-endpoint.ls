require! {
  'ws' : WebSocket
}
debug = require('debug')('mock-websocket-endpoint')


# Sends and receives WebSocket messages in tests
class WebSocketEndpoint

  (@name) ->

    @received-messages = []
    @socket = null


  listen: (@exocom-port) ~>
    @socket = new WebSocket "ws://localhost:#{@exocom-port}"
      ..on 'error', (error) ~>
        console.log error
      ..on 'open', ~>
        data =
          name: "exocom.register-service"
          sender: "#{@name}"
        @send data
      ..on 'message', (data) ~>
        @_on-socket-message data


  close: ~>
    @socket?.close!


  send: (request-data) ~>
    @socket.send JSON.stringify request-data


  _on-socket-message: (data) ~>
    @received-messages.push JSON.parse data.to-string!



module.exports = WebSocketEndpoint
