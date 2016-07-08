require! {
  'zmq'
}
debug = require('debug')('mock-zmq-endpoint')


# Sends and receives ZMQ messages in tests
class ZmqEndpoint

  ({push-port, pull-port} = {}) ->
    @pull-socket = null
    @push-socket = null
    @received-messages = []

    if push-port
      @push-socket = zmq.socket 'push'
        ..connect "tcp://localhost:#{push-port}"

    if pull-port
      @pull-socket = zmq.socket 'pull'
        ..bind-sync "tcp://*:#{pull-port}"
        ..on 'message', @_on-pull-socket-message


  close: ~>
    @push-socket?.close!
    @pull-socket?.close!


  send: (request-data) ~>
    @push-socket.send JSON.stringify request-data


  _on-pull-socket-message: (data) ~>
    @received-messages.push JSON.parse data.to-string!



module.exports = ZmqEndpoint
