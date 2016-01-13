require! {
  'node-uuid' : uuid
  'request'
}
debug = require('debug')('exorelay:command-sender')


# Subsystem for sending commands out to Exosphere
class CommandSender

  ({@exocomm-port} = {}) ->


  send: (obj) ->
    debug "sending command '#{obj.command}'"
    options =
      method: 'POST'
      url: "http://localhost:#{@exocomm-port}/send/#{obj.command}"
      json: yes
      body:
        'request-id': uuid.v1!
    options.body.payload = obj.payload if obj.payload
    options.body['replying-to'] = obj.replying-to if obj.replying-to
    request options, (err, response, body) ->
      if err || (response?.status-code isnt 200)
        debug "Error sending command '#{obj.command}'"
        debug "* err: #{err}"
        debug "* response: #{response?.status-code}"
    options.body['request-id']



module.exports = CommandSender
