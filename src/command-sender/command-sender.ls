require! {
  'request'
}
debug = require('debug')('exorelay:command-sender')


# Subsystem for sending commands out to Exosphere
class CommandSender

  ({@exo-messaging-port} = {}) ->


  send: (command, done) ->
    debug "sending command '#{command}'"
    options =
      method: 'POST'
      url: "http://localhost:#{@exo-messaging-port}/send/#{command}"
    request options, (err, @exo-messaging-response, body) ~>
      done!



module.exports = CommandSender
