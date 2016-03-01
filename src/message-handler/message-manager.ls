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
    @message-handlers = new HandlerRegistry 'message-handler'
    @reply-handlers = new HandlerRegistry 'reply-handler'

    delegate \hasHandler \registerHandler \registerHandlers from: @, to: @message-handlers
    delegate-event 'error', from: [@message-handlers, @reply-handlers], to: @


  # Handles the given message with the given payload.
  # Return whether the request was handled or not.
  handle-request: (message-data, methods) ->
    | !message-data.request-id                              =>  'missing request id'
    | @reply-handlers.has-handler message-data.response-to  =>  @reply-handlers.handle(message-data.response-to, message-data) && 'success'
    | @message-handlers.has-handler message-data.message    =>  @message-handlers.handle(message-data.message, message-data, methods) && 'success'
    | otherwise                                             =>  'unknown message'


  register-reply-handler: (request-id, handler) ->
    @reply-handlers.register-handler request-id, handler



module.exports = HandlerManager
