require! {
  'ws' : WebSocket
}
debug = require('debug')('mock-service')


# A class to encapsulate the functionality of a service in the ExoSphere
# that can send, receive, and track messages using WebSockets.
class MockService

  ({port, name, namespace} = {}) ->
    @received-messages = []


    if port
      @socket = new WebSocket "ws://localhost:#{port}/services"
        ..on 'message', @_on-message
        ..on 'error', (error) ~>
          console.log error
        ..on 'open', ~>
          @send do
            name: 'exocom.register-service'
            sender: name
            payload:
              name: name
              internal-namespace: namespace
            id: '123'


  close: ~>
    | @closed => return
    @socket?.close!
    @closed = yes


  send: (request-data) ~>
    @socket.send JSON.stringify request-data

  _on-message: (data) ~>
    @received-messages.unshift(JSON.parse data.to-string!)


module.exports = MockService
