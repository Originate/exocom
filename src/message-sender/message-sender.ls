require! {
  'events' : {EventEmitter}
  'lodash.isempty' : is-empty
  'node-uuid' : uuid
  'request'
}
debug = require('debug')('exorelay:message-sender')


# Subsystem for sending messages out to Exosphere
class MessageSender extends EventEmitter

  ({@service-name, @exocom-port} = {}) ->

    @exocom-port = +@exocom-port

    # Contains the id of the most recently sent request (for testing)
    @last-sent-id = null


  # Returns a method that sends a reply to the message with the given request
  #
  reply-method-for: (id) ->
    | !id  =>  return @emit 'error', new Error 'MessageSender.replyMethodFor needs a id'

    (message-name, payload = {}) ~>
      @send message-name, payload, response-to: id


  send: (message-name, payload = {}, options = {}) ->
    | !message-name                      =>  return @emit 'error', new Error 'ExoRelay#send cannot send empty messages'
    | typeof message-name isnt 'string'  =>  return @emit 'error', new Error 'ExoRelay#send can only send string messages'
    | typeof payload is 'function'       =>  return @emit 'error', new Error 'ExoRelay#send cannot send functions as payload'

    @_log message-name, options
    request-data =
      method: 'POST'
      url: "http://localhost:#{@exocom-port}/send/#{message-name}"
      json: yes
      body:
        sender: @service-name
        id: uuid.v1!
    request-data.body.payload = payload unless is-empty payload
    request-data.body.response-to = options.response-to if options.response-to
    request request-data, (err, response, body) ->
      if err || (response?.status-code isnt 200)
        debug "Error sending message '#{message-name}'"
        debug "* err: #{err}"
        debug "* response: #{response?.status-code}"
    @last-sent-id = request-data.body.id


  _log: (message-name, options) ->
    | options.response-to  =>  debug "sending message '#{message-name}' in response to '#{options.response-to}'"
    | _                    =>  debug "sending message '#{message-name}'"



module.exports = MessageSender
