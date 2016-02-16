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
    debug "sending '#{command.name}' to '#{service.name}'"
    request request-data, done



module.exports = CommandSender
