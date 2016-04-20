require! {
  'request'
}
debug = require('debug')('exocom:message-sender')


class MessageSender

  # Sends the given message to the given services
  send-to-services: (message-data, services) ->
    for service in services
      @send-to-service message-data, service


  send-to-service: (message-data, service, done) ->
    translated-message-name = @_translate message-data.name, for: service
    request-data =
      url: "http://localhost:#{service.port}/run/#{translated-message-name}"
      method: 'POST'
      body:
        id: message-data.id
        payload: message-data.payload
      json: yes
    request-data.body.response-to = message-data.response-to if message-data.response-to
    @_log message-data, service
    request request-data, done
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
