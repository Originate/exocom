require! {
  'events' : {EventEmitter}
  'lodash.isempty' : is-empty
  'node-uuid' : uuid
  'request'
}
debug = require('debug')('exorelay:message-sender')


# Subsystem for sending messages out to Exosphere
class MessageSender extends EventEmitter

  ({@exocomm-port} = {}) ->

    @exocomm-port = +@exocomm-port

    # Contains the request-id of the most recently sent request (for testing)
    @last-sent-request-id = null


  # Returns a method that sends a reply to the message with the given request
  #
  reply-method-for: (request-id) ->
    | !request-id  =>  return @emit 'error', new Error 'MessageSender.replyMethodFor needs a requestId'

    (message, payload = {}) ~>
      @send message, payload, response-to: request-id


  send: (message, payload = {}, options = {}) ->
    | !message                      =>  return @emit 'error', new Error 'ExoRelay#send cannot send empty messages'
    | typeof message isnt 'string'  =>  return @emit 'error', new Error 'ExoRelay#send can only send string messages'
    | typeof payload is 'function'  =>  return @emit 'error', new Error 'ExoRelay#send cannot send functions as payload'

    @_log message, options
    request-data =
      method: 'POST'
      url: "http://localhost:#{@exocomm-port}/send/#{message}"
      json: yes
      body:
        requestId: uuid.v1!
    request-data.body.payload = payload unless is-empty payload
    request-data.body.response-to = options.response-to if options.response-to
    request request-data, (err, response, body) ->
      if err || (response?.status-code isnt 200)
        debug "Error sending message '#{message}'"
        debug "* err: #{err}"
        debug "* response: #{response?.status-code}"
    @last-sent-request-id = request-data.body.request-id


  _log: (message, options) ->
    | options.response-to  =>  debug "sending message '#{message}' in response to '#{options.response-to}'"
    | _                    =>  debug "sending message '#{message}'"

module.exports = MessageSender
