require! {
  'ws' : WebSocket
}
debug = require('debug')('mock-websocket-endpoint')


# Sends and receives WebSocket messages in tests
class WebSocketEndpoint

  (@name) ->
    @received-messages = []
    @socket = null


  connect: (@exocom-port) ~>
    @socket = new WebSocket "ws://localhost:#{@exocom-port}"
      ..on 'error', @_on-socket-error
      ..on 'open', @_on-socket-open
      ..on 'message', @_on-socket-message


  can-send: ->
    @socket.ready-state is WebSocket.OPEN


  close: ~>
    @socket?.close!


  send: (request-data) ~>
    @socket.send JSON.stringify request-data


  _on-socket-error: (error) ->
    console.log error


  _on-socket-message: (data) ~>
    @received-messages.push JSON.parse data.to-string!


  _on-socket-open: ~>
    @send do
      name: \exocom.register-service
      sender: @name



module.exports = WebSocketEndpoint
