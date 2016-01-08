require! {
  'request'
}
debug = require('debug')('exorelay:command-sender')


# Subsystem for sending commands out to Exosphere
class CommandSender

  ({@exo-messaging-port} = {}) ->


  send: (obj, done) ->
    match typeof obj
    | 'string'  =>  @_send obj, null, done
    | 'object'  =>  @_send obj.command, obj.payload, done
    | _         =>  throw new Error "Unknown thing to send: #{obj}"


  _send: (command, payload = {}, done) ->
    debug "sending command '#{command}'"
    options =
      method: 'POST'
      url: "http://localhost:#{@exo-messaging-port}/send/#{command}"
      json: yes
      body: payload
    request options, (err, @exo-messaging-response, body) ~>
      done!



module.exports = CommandSender
