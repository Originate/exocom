require! {
  'debug'
  'events' : {EventEmitter}
}


# A registry for command handlers of a particular type
class HandlerRegistry extends EventEmitter

  (debug-name) ->

    # handler functions for incoming requests
    @handlers = {}

    @debug = debug "exorelay:#{debug-name}"


  # Returns the handler for the request with the given id,
  # or undefined if not found.
  get-handler: (request-id) ->
    @handlers[request-id]

  # handlers the request with the given request-id.
  # returns whether the request was handled or not
  handle: (request-id, request-data, methods) ->
    if handler = @get-handler request-id
      @debug "handling command '#{request-id}'"
      handler request-data, methods
    !!handler


  # Returns whether this RequestManager has a handler for
  # the request with the given id registered.
  has-handler: (request-id) ->
    typeof @get-handler(request-id) is 'function'


  register-handler: (request-id, handler) ->
    | !request-id                      =>  return @emit 'error', new Error 'No request id provided'
    | typeof request-id isnt 'string'  =>  return @emit 'error', new Error 'Request ids must be strings'
    | !handler                         =>  return @emit 'error', new Error 'No command handler provided'
    | typeof handler isnt 'function'   =>  return @emit 'error', new Error 'Command handler must be a function'
    | @has-handler request-id          =>  return @emit 'error', new Error "There is already a handler for command '#{request-id}'"

    @debug "registering handler for request-id '#{request-id}'"
    @handlers[request-id] = handler


  register-handlers: (handlers) ->
    for request-id, handler of handlers
      @register-handler request-id, handler



module.exports = HandlerRegistry
