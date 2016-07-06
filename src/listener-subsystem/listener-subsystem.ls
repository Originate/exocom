require! {
  'events' : {EventEmitter}
  './http-listener' : HttpListener
  'rails-delegate' : {delegate, delegate-event}
  './zmq-listener' : ZMQListener
}
debug = require('debug')('exocom-dev:message-manager')


# The message handling subsystem.
#
# Handles all types of messages
class ListenerSubsystem extends EventEmitter

  (@exocom) ->
    @http-listener = new HttpListener @exocom
    @zmq-listener = new ZMQListener @exocom

    delegate-event 'zmq-bound' 'http-bound' 'error', from: [@http-listener, @zmq-listener], to: @


  # Bind the subclasses to their respective ports
  listen: ({http-port, zmq-port}) ->
    @http-listener.listen http-port
    @zmq-listener.listen zmq-port


  # Closes all open ports
  close: !->
    @http-listener.close!
    @zmq-listener.close!


  # Get the port that is accepting HTTP requests
  http-port: ->
    @http-listener.port


  # Get the port that services are sending/recieving on
  zmq-port: ->
    @zmq-listener.port



module.exports = ListenerSubsystem
