require! {
  'request'
}
debug = require('debug')('exorelay:command-sender')


# Subsystem for sending commands out to Exosphere
class CommandSender

  ({@exo-messaging-port} = {}) ->


  send: (obj) ->
    debug "sending command '#{obj.command}'"
    options =
      method: 'POST'
      url: "http://localhost:#{@exo-messaging-port}/send/#{obj.command}"
      json: yes
      body: {}
    options.body.payload = obj.payload if obj.payload
    options.body['replying-to'] = obj.replying-to if obj.replying-to
    request options, (err, response, body) ->
      if err or response.status-code isnt 200
        debug "Error sending command '#{obj.command}': err = '#{err}', response = '#{response.status-code}'"



module.exports = CommandSender
