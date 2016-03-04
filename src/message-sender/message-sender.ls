require! {
  'request'
}
debug = require('debug')('exocom:message-sender')


class MessageSender

  # Sends the given message to the given services
  send-to-services: (message, services) ->
    for service in services
      @send-to-service message, service


  send-to-service: (message, service, done) ->
    request-data =
      url: "http://localhost:#{service.port}/run/#{message.name}"
      method: 'POST'
      body:
        id: message.id
        payload: message.payload
      json: yes
    request-data.body.response-to = message.response-to if message.response-to
    @_log message, service
    request request-data, done


  _log: ({name, id, response-to}, service) ->
    | response-to  =>  debug "sending '#{name}' with id '#{id}' in response to '#{response-to}' to '#{service.name}'"
    | _            =>  debug "sending '#{name}' with id '#{id}' to '#{service.name}'"



module.exports = MessageSender
