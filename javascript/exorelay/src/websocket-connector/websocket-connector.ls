require! {
  'events' : {EventEmitter}
  'uuid' : uuid
  'wait' : {wait}
  'ws' : WebSocket
}
debug = require('debug')('exorelay:websocket-listener')


# The WebSocket endpoint which the Exosphere environment can
# send messages and receive messages from.
#
# Emits these events:
# - online
# - offline
# - error
class WebSocketConnector extends EventEmitter

  ({@exocom-host, @role, @exocom-port} = {}) ->
    @exocom-port = +@exocom-port

    # Contains the id of the most recently sent request (for testing)
    @last-sent-message = null


  # Closes the port that ExoRelay is communicating on
  close: (done) ->
    return done! unless @socket
    debug "no longer connected at 'ws://#{@exocom-host}/#{@exocom-port}'"
    @should-reconnect-on-socket-closed = no
    switch @socket.ready-state
      | WebSocket.CONNECTING =>
          @socket.terminate!
          done!
      | WebSocket.OPEN =>
          @socket.on 'close', done
          @socket.close!
      | WebSocket.CLOSING => @socket.on 'close', done
      | WebSocket.CLOSED => done!


  # Returns a method that sends a reply to the message with the given request
  reply-method-for: (activity-id, auth, is-security) ->
    | !activity-id  =>  return @emit 'error', new Error 'WebSocketConnector.replyMethodFor needs an activity-id'
    (message-name, payload = {}) ~>
      @send message-name, payload, activity-id: activity-id, auth: auth, is-security: is-security


  send: (message-name, payload, options = {}) ->
    | !message-name                      =>  return @emit 'error', new Error 'ExoRelay#send cannot send empty messages'
    | typeof message-name isnt 'string'  =>  return @emit 'error', new Error 'ExoRelay#send can only send string messages'
    | typeof payload is 'function'       =>  return @emit 'error', new Error 'ExoRelay#send cannot send functions as payload'

    @_log-sending message-name, options
    request-data =
      name: message-name
      sender: @role
      id: uuid.v1!
      activity-id: options.activity-id or uuid.v1!
    request-data.payload = payload if payload?
    request-data.auth = options.auth if options.auth
    request-data.is-security = options.is-security if options.is-security
    @socket.send JSON.stringify request-data
    @last-sent-message = request-data


  connect: ~>
    @start-connect-time = Date.now()
    @log-connect-error-delay = 1000
    @should-reconnect-on-socket-closed = yes
    @should-use-internal-reconnect = true
    @_internalConnect!


  _internalConnect: ->
    @socket = new WebSocket "ws://#{@exocom-host}:#{@exocom-port}/services"
      ..on 'open', @_on-socket-open
      ..on 'message', @_on-socket-message
      ..on 'error', @_on-socket-error
      ..on 'close', @_on-socket-close


  _on-socket-close: ~>
    if @should-reconnect-on-socket-closed
      if @should-use-internal-reconnect
        setTimeout (~> @_internalConnect!), 100
      else
        @connect!
    else
      @emit 'offline'


  _on-socket-open: ~>
    @should-use-internal-reconnect = false
    @emit 'online'


  _on-socket-error: (error) ~>
    | error.errno is 'EADDRINUSE' => @emit 'error', "port #{@exocom-port} is already in use"
    | @socket.ready-state is WebSocket.CONNECTING =>
      if Date.now() > @start-connect-time + @log-connect-error-delay
        @emit 'error', "Could not connect after #{@log-connect-error-delay}ms: #{error.message}"
        @log-connect-error-delay = @log-connect-error-delay * 2
    | otherwise => @emit 'error', error


  _on-socket-message: (data) ~>
    request-data = data |> JSON.parse |> @_parse-request
    @_log-received request-data
    switch (result = @listeners('message')[0] request-data)
      | 'success'             =>
      | 'missing message id'  =>  @emit 'error', Error 'missing message id'
      | 'unknown message'     =>  @emit 'error', Error "unknown message: '#{request-data.message-name}'"
      | _                     =>  @emit 'error', Error "unknown result code: '#{result}'"


  _log-received: ({message-name, id, activity-id}) ->
    debug "received message '#{message-name}' with id '#{id}' in discussion of '#{activity-id}'"


  _log-sending: (message-name, options) ->
    debug "sending message '#{message-name}' in discussion of '#{options.activity-id}'"


  # Returns the relevant data from a request
  _parse-request: (req) ->
    {
      message-name: req.name
      payload: req.payload
      activity-id: req.activity-id
      id: req.id
      auth: req.auth
      is-security: req.is-security
    }



module.exports = WebSocketConnector
