require! {
  'events' : {EventEmitter}
  '../message-cache/message-cache' : MessageCache
  'zmq'
}
debug = require('debug')('exocom:message-sender')

class MessageSender extends EventEmitter

  (@exocom)->
    # Stores the service names and respective push socket
    @service-sockets = {}


  # Removes all existing connections
  # Connects our outgoing socket to all existing services
  bind-services: (services) ->
    @clear-ports!
    for service-name, params of services
      @bind-new-service {service-name, params.host, params.port}


  # Connects our outgoing socket to all existing services
  bind-new-service: ({service-name, host, port}) ->
    @service-sockets[service-name] = zmq.socket 'push'
      ..connect "tcp://#{host}:#{port}"
      ..monitor 10, 0
      ..on 'disconnect', ~>
        @exocom.remove-routing-config {service-name, host}


  unbind-service: ({service-name}) ->
    @service-sockets[service-name]?.close!
    delete @service-sockets[service-name]


  # Closes and deletes all existing push sockets
  clear-ports: ->
    for service, socket of @service-sockets
      socket.close!
      delete @service-sockets[service]


  # Sends the given message to the given services
  send-to-services: (message-data, services) ->
    for service in services
      @send-to-service message-data, service


  send-to-service: (message-data, service, done) ->
    translated-message-name = @_translate message-data.name, for: service
    request-data =
      name: translated-message-name
      id: message-data.id
      payload: message-data.payload
      timestamp: message-data.timestamp
    if message-data.response-to
      request-data.response-time = message-data.response-time
      request-data.response-to = message-data.response-to
    @_log message-data, service
    @service-sockets[service.name].send JSON.stringify request-data
    done?!
    result = {[key, value] for key, value of message-data}
    result.name = translated-message-name
    result


  _log: ({name, id, response-to}, service) ->
    | response-to  =>  debug "sending '#{name}' with id '#{id}' in response to '#{response-to}' to '#{service.name}'"
    | _            =>  debug "sending '#{name}' with id '#{id}' to '#{service.name}'"


  _translate: (message-name, {for: service}) ->
    message-parts = message-name.split '.'
    switch
      | !service.internal-namespace                     =>  message-name
      | message-parts.length is 1                       =>  message-name
      | message-parts[0] is service.internal-namespace  =>  message-name
      | otherwise                                       => "#{service.internal-namespace}.#{message-parts[1]}"



module.exports = MessageSender
