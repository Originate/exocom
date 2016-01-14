require! {
  'lodash.isempty' : is-empty
  'node-uuid' : uuid
  'request'
}
debug = require('debug')('exorelay:command-sender')


# Subsystem for sending commands out to Exosphere
class CommandSender

  ({@exocomm-port} = {}) ->

    # Contains the request-id of the most recently sent request (for testing)
    @last-sent-request-id = null


  # Returns a method that sends a reply to the command with the given request
  #
  reply-method-for: (request-id) ->
    | !request-id  =>  throw new Error 'CommandSender.replyMethodFor needs a requestId'

    (command, payload = {}) ~>
      @send command, payload, response-to: request-id


  send: (command, payload = {}, options = {}) ->
    @_log command, options
    request-data =
      method: 'POST'
      url: "http://localhost:#{@exocomm-port}/send/#{command}"
      json: yes
      body:
        requestId: uuid.v1!
    request-data.body.payload = payload unless is-empty payload
    request-data.body.response-to = options.response-to if options.response-to
    request request-data, (err, response, body) ->
      if err || (response?.status-code isnt 200)
        debug "Error sending command '#{command}'"
        debug "* err: #{err}"
        debug "* response: #{response?.status-code}"
    @last-sent-request-id = request-data.body.request-id


  _log: (command, options) ->
    | options.response-to  =>  debug "sending command '#{command}' in response to '#{options.response-to}'"
    | _                    =>  debug "sending command '#{command}'"

module.exports = CommandSender
