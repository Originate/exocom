require! {
  'node-uuid' : uuid
  'ws' : {Server: WebSocketServer}
}
debug = require('debug')('exocom-mock')


# ASends and receives ZMQ messages in tests
class MockExoCom

  ->
    @server = null
    @port = null
    @service-sockets = {}
    @received-messages = []
    @receive-callback = null


  close: ~>
    @server?.close!


  listen: (+@port) ~>
    @server = new WebSocketServer {port: @port}
      ..on 'error', (error) ~>
        console.log error
      ..on 'connection', (websocket) ~>
        websocket.on 'message', (message) ~>
          @on-socket-message message, websocket


  on-socket-message: (message, websocket) ->
    request-data = JSON.parse message
    if request-data.name is "exocom.register-service"
      @register-service {name: request-data.sender, websocket}
    @_on-message request-data


  on-receive: (@receive-callback) ~>
    if @received-messages.length
      @receive-callback!


  register-service: ({name, websocket}) ~>
    @service-sockets[name] = websocket


  reset: ~>
    @received-messages = []


  send: ({service, name, payload, message-id, response-to}) ~>
    | !@service-sockets[service]  =>  throw new Error "unknown service: '#{service}'"

    @received-messages = []
    request-data =
      name: name
      payload: payload
      id: message-id or uuid.v1!
    request-data.response-to = response-to if response-to
    @service-sockets[service].send JSON.stringify request-data


  _on-message: (data) ~>
    @received-messages.push data
    @receive-callback?!



module.exports = MockExoCom
