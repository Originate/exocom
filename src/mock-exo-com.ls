require! {
  'node-uuid' : uuid
  'zmq'
}
debug = require('debug')('exocom-mock')


# ASends and receives ZMQ messages in tests
class MockExoCom

  ->
    @push-sockets = {}
    @pull-socket = null
    @pull-socket-port = null
    @received-messages = []
    @receive-callback = null


  close: ~>
    for service, socket of @push-sockets
      socket.close!
      delete @push-sockets[service]
    @pull-socket?.close!


  listen: (+@pull-socket-port) ~>
    @pull-socket = zmq.socket 'pull'
      ..bind-sync "tcp://*:#{@pull-socket-port}"
      ..on 'message', @_on-pull-socket-message


  on-receive: (@receive-callback) ~>
    if @received-messages.length
      @receive-callback!


  register-service: ({name, port}) ~>
    @push-sockets[name] = zmq.socket 'push'
      ..connect "tcp://localhost:#{port}"


  reset: ~>
    @received-messages = []


  send: ({service, name, payload, message-id, response-to}) ~>
    | !@push-sockets[service]  =>  throw new Error "unknown service: '#{service}'"

    @received-messages = []
    request-data =
      name: name
      payload: payload
      id: message-id or uuid.v1!
    request-data.response-to = response-to if response-to
    @push-sockets[service].send JSON.stringify request-data


  _on-pull-socket-message: (data) ~>
    @received-messages.push JSON.parse data.to-string!
    @receive-callback?!



module.exports = MockExoCom
