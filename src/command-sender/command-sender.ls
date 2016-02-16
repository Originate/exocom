require! {
  'request'
}
debug = require('debug')('exocomm:command-sender')


class CommandSender

  # Sends the given command to the given services
  send-to-services: (command, services) ->
    for service in services
      @send-to-service command, service


  send-to-service: (command, service, done) ->
    request-data =
      url: "http://localhost:#{service.port}/run/#{command.name}"
      method: 'POST'
      body:
        requestId: command.request-id
        payload: command.payload
      json: yes
    request-data.body.response-to = command.response-to if command.response-to
    @_log command, service
    request request-data, done


  _log: ({name, request-id, response-to}, service) ->
    | response-to  =>  debug "sending '#{name}' with id '#{request-id}' in response to '#{response-to}' to '#{service.name}'"
    | _            =>  debug "sending '#{name}' with id '#{request-id}' to '#{service.name}'"



module.exports = CommandSender
