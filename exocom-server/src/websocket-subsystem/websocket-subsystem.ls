require! {
  'chalk' : {cyan}
  'events' : {EventEmitter}
  '../message-cache/message-cache' : MessageCache
  'ws' : {Server: WebSocketServer}
}
debug = require('debug')('exocom:websocket-subsystem')


# The web sockets endpoint that listens/sends messages from/to services
#
# Emits these events:
# - online: when it is online and ready to go
# - error: for critical issues
# - warn: for non-critical issues
class WebSocketSubsystem extends EventEmitter

  ({@exocom, @logger} = {}) ->
    @server = null
    @port = null

    # Stores the service names and respective web socket
    #
    # format:
    # {
    #   'client 1 name': websocket,
    #   'client 2 name': websocket,
    #   ...
    # }
    @sockets = {}


  close: ->
    | !@server  =>  return
    debug 'websockets going offline'
    @server.close!


  deregister-client: (client-name) ->
    @sockets[client-name]?.close!
    delete @sockets[client-name]


  invalid-sender: (sender, message-name) ->
    !@exocom.client-registry.can-send sender, message-name


  # Listens at the given port
  # by hooking into the given http server instance
  listen: (@port, server) ->
    @server = new WebSocketServer {server, path: '/services'}
      ..on 'connection', @on-connection
      ..on 'listening', ~>
        @logger.log "ExoCom WebSocket listener online at port #{cyan port}"
        @emit 'online', @port
      ..on 'error', (err) ~> @logger.error err


  # called when a new service instance connects
  on-connection: (websocket) ~>
    websocket.on 'message', (message) ~>
      @on-message message, websocket


  # called when a new message from a service instance arrives
  on-message: (message-text, websocket) ->
    message = JSON.parse(message-text)
    @_log-received message
    switch
      | message.name is \exocom.register-service      =>  @on-service-instance-registration message.payload, websocket
      | @invalid-sender message.sender, message.name  =>  @logger.error "Service '#{message.sender}' is not allowed to broadcast the message '#{message.name}'"
      | otherwise                                     =>  @on-normal-message-receive message


  # called when a service instance sends a normal message
  # i.e. not a registration message
  on-normal-message-receive: (message) ->
    switch (result = @exocom.send-message message)
      | 'success'             =>
      | 'no receivers'        =>  @logger.warning "No receivers for message '#{message.name}' registered"
      | 'missing request id'  =>  @logger.error 'missing request id'
      | 'unknown message'     =>  @logger.error "unknown message: '#{request-data.message}'"
      | _                     =>  @logger.error "unknown result code: '#{@result}'"


  # called when a service instance registers itself with Exocom
  on-service-instance-registration: (payload, websocket) ->
    @exocom.register-client payload, websocket
    @register-client client-name: payload.client-name, websocket: websocket


  # Registers the given websocket as a connection
  # to an instance of the service with the given name
  register-client: ({client-name, websocket}) ->
    @sockets[client-name] = websocket
      ..on 'close', ~>
        @exocom.deregister-client client-name
        @deregister-client client-name


  send-message-to-service: (message, service) ->
    internal-message-name = @_internal-message-name message.name, for: service
    request-data =
      name: internal-message-name
      id: message.id
      payload: message.payload
      timestamp: message.timestamp
    if message.response-to
      request-data.response-time = message.response-time
      request-data.response-to = message.response-to
    @_log-sending message, service
    @sockets[service.client-name].send JSON.stringify request-data
    result = {[key, value] for key, value of message}
    result.name = internal-message-name
    result


  send-message-to-services: (message-data, services) ->
    for service in services
      @send-message-to-service message-data, service



  # Translates outgoing message into one that the receiving service will understand
  _internal-message-name: (message-name, {for: service}) ->
    message-parts = message-name.split '.'
    switch
      | !service.internal-namespace                     =>  message-name
      | message-parts.length is 1                       =>  message-name
      | message-parts[0] is service.internal-namespace  =>  message-name
      | otherwise                                       => "#{service.internal-namespace}.#{message-parts[1]}"


  _log-received: (message) ->
    | message.response-to  =>  debug "received '#{message.name}' with id '#{message.id}' in response to '#{message.response-to}'"
    | _                    =>  debug "received '#{message.name}' with id '#{message.id}'"


  _log-sending: (message, service) ->
    | message.response-to  =>  debug "sending '#{message.name}' with id '#{message.id}' in response to '#{message.response-to}' to '#{service.name}'"
    | _                    =>  debug "sending '#{message.name}' with id '#{message.id}' to '#{service.name}'"



module.exports = WebSocketSubsystem
