require! {
  './handler-registry' : HandlerRegistry
}
debug = require('debug')('exorelay:command-manager')


# The command handling subsystem.
#
# Handles all types of commands
class HandlerManager

  ->
    @command-handlers = new HandlerRegistry 'command-handler'
    @reply-handlers = new HandlerRegistry 'reply-handler'



  # Handles the given command with the given payload.
  # Return whether the request was handled or not.
  handle-request: ({command, replying-to, payload}) ->
    | @reply-handlers.has-handler replying-to  =>  @reply-handlers.handle replying-to, payload
    | @command-handlers.has-handler command    =>  @command-handlers.handle command, payload
    | otherwise                                =>  debug "no handler found for command '#{command}' and request-id '#{replying-to}'"


  has-handler: (command) ->
    @command-handlers.has-handler command


  register-handler: (command, handler) ->
    @command-handlers.register-handler command, handler


  register-handlers: (handlers) ->
    @command-handlers.register-handlers handlers


  register-reply-handler: (request-id, handler) ->
    @reply-handlers.register-handler request-id, handler



module.exports = HandlerManager
