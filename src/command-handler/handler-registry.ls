debug = require('debug')('exorelay:command-handlers')


# A registry for command handlers of a particular type
class HandlerRegistry

  ->

    # handler functions for incoming requests
    @handlers = {}


  # Returns the handler for the request with the given id,
  # or undefined if not found.
  get-handler: (request-id) ->
    @handlers[request-id]

  # handlers the request with the given request-id.
  # returns whether the request was handled or not
  handle: (request-id, request-data) ->
    if handler = @get-handler(request-id)
      debug "found handler for command '#{request-id}'"
      handler request-data
    !!handler

  # Returns whether this RequestManager has a handler for
  # the request with the given id registered.
  has-handler: (request-id) ->
    typeof @get-handler(request-id) is 'function'


  register-handler: (request-id, handler) ->
    debug "registering handler for request-id '#{request-id}'"
    @handlers[request-id] = handler


  register-handlers: (handlers) ->
    for request-id, handler of handlers
      @register request-id, handler



module.exports = HandlerRegistry
