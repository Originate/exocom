require! {
  './handler-registry' : HandlerRegistry
}
debug = require('debug')('exorelay:command-manager')


# The command handling subsystem.
#
# Handles all types of commands
class HandlerManager

  ->
    @command-handlers = new HandlerRegistry!



  # Handles the given command with the given payload.
  # Return whether the request was handled or not.
  handle-request: (command, payload) ->
    if result = @command-handlers.handle command, payload
      debug "handled command '#{command}'"
    else
      debug "no handler found for command '#{command}'"
    result


  register-handler: (command, handler) ->
    @command-handlers.register-handler command, handler


  register-handlers: (handlers) ->
    @command-handlers.register-handlers handlers



module.exports = HandlerManager
