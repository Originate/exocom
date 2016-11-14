require! {
  'ws' : WebSocket
}
debug = require('debug')('mock-service')


# A class to encapsulate the functionality of a service in the ExoSphere
# that can send, receive, and track messages using WebSockets.
class MockService

  ({@port, @name, @namespace} = {}) ->
    @received-messages = []


    if @port
      @socket = new WebSocket "ws://localhost:#{@port}/services"
        ..on 'message', @_on-socket-message
        ..on 'error', @_on-socket-error
        ..on 'open', @_on-socket-open


  close: ~>
    | @closed => return
    @socket?.close!
    @closed = yes


  send: (request-data) ~>
    @socket.send JSON.stringify request-data


  _on-socket-error: (error) ~>
    console.log error


  _on-socket-message: (data) ~>
    @received-messages.unshift(JSON.parse data.to-string!)


  _on-socket-open: ~>
    @send do
      name: 'exocom.register-service'
      sender: @name
      payload:
        name: @name
      id: '123'



module.exports = MockService
