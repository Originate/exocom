require! {
  'events' : {EventEmitter}
  'zmq'
}
debug = require('debug')('exocom:message-sender')


class MessageSender extends EventEmitter

  ->
    # Stores the service names and respective push socket
    @service-sockets = {}

  # Connects our outgoing socket to all existing services
  bind-services: (services) ->
    @clear-ports!
    for service, params of services
      @service-sockets[service] = zmq.socket 'push'
        ..connect "tcp://#{params.host}:#{params.port}"


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
    request-data.response-to = message-data.response-to if message-data.response-to
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
