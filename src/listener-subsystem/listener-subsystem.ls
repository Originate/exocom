require! {
  'events' : {EventEmitter}
  './http-listener' : HttpListener
  'rails-delegate' : {delegate, delegate-event}
}
debug = require('debug')('exocom-dev:message-manager')


# The message handling subsystem.
#
# Handles all types of messages
class ListenerSubsystem extends EventEmitter

  (@exocom) ->
    @http-listener = new HttpListener @exocom

    delegate-event 'http-bound' 'error', from: [@http-listener], to: @


  # Bind the subclasses to their respective ports
  listen: ({http-port}) ->
    @http-listener.listen http-port


  # Closes all open ports
  close: !->
    @http-listener.close!


  # Get the port that is accepting HTTP requests
  http-port: ->
    @http-listener.port


module.exports = ListenerSubsystem
