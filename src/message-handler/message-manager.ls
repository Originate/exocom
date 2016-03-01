require! {
  'events' : {EventEmitter}
  './handler-registry' : HandlerRegistry
  'rails-delegate' : {delegate, delegate-event}
}
debug = require('debug')('exorelay:message-manager')


# The message handling subsystem.
#
# Handles all types of messages
class HandlerManager extends EventEmitter

  ->
    @command-handlers = new HandlerRegistry 'message-handler'
    @reply-handlers = new HandlerRegistry 'reply-handler'

    delegate \hasHandler \registerHandler \registerHandlers from: @, to: @command-handlers
    delegate-event 'error', from: [@command-handlers, @reply-handlers], to: @


  # Handles the given message with the given payload.
  # Return whether the request was handled or not.
  handle-request: (message-data, methods) ->
    | !message-data.request-id                                =>  'missing request id'
    | @reply-handlers.handle-reply message-data               =>  'success'
    | @command-handlers.handle-command message-data, methods  =>  'success'
    | otherwise                                               =>  'unknown message'


  register-reply-handler: (request-id, handler) ->
    @reply-handlers.register-handler request-id, handler



module.exports = HandlerManager
