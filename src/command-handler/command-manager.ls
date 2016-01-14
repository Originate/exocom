require! {
  './handler-registry' : HandlerRegistry
  'rails-delegate' : delegate
}
debug = require('debug')('exorelay:command-manager')


# The command handling subsystem.
#
# Handles all types of commands
class HandlerManager

  ->
    @command-handlers = new HandlerRegistry 'command-handler'
    @reply-handlers = new HandlerRegistry 'reply-handler'

    delegate \hasHandler \registerHandler \registerHandlers from: @, to: @command-handlers


  # Handles the given command with the given payload.
  # Return whether the request was handled or not.
  handle-request: ({command, response-to, payload}, methods) ->
    | @reply-handlers.has-handler response-to  =>  @reply-handlers.handle response-to, payload
    | @command-handlers.has-handler command    =>  @command-handlers.handle command, payload, methods
    | otherwise                                =>  debug "no handler found for command '#{command}' and request-id '#{response-to}'"


  register-reply-handler: (request-id, handler) ->
    @reply-handlers.register-handler request-id, handler



module.exports = HandlerManager
