require! {
  'events' : {EventEmitter}
  'lodash.isempty' : is-empty
  'node-uuid' : uuid
  'zmq'
}
debug = require('debug')('exorelay:message-sender')


# Subsystem for sending messages out to Exosphere
class MessageSender extends EventEmitter

  ({@exocom-host, @service-name, @exocom-port} = {}) ->

    @exocom-port = +@exocom-port
    @socket = zmq.socket 'push'
      ..connect "tcp://#{@exocom-host}:#{@exocom-port}"

    # Contains the id of the most recently sent request (for testing)
    @last-sent-id = null


  # Closes the port that ExoRelay is sending messages on
  close-port: ->
    @socket.close!


  # Returns a method that sends a reply to the message with the given request
  reply-method-for: (id) ->
    | !id  =>  return @emit 'error', new Error 'MessageSender.replyMethodFor needs a id'

    (message-name, payload) ~>
      @send message-name, payload, response-to: id


  send: (message-name, payload, options = {}) ->
    | !message-name                      =>  return @emit 'error', new Error 'ExoRelay#send cannot send empty messages'
    | typeof message-name isnt 'string'  =>  return @emit 'error', new Error 'ExoRelay#send can only send string messages'
    | typeof payload is 'function'       =>  return @emit 'error', new Error 'ExoRelay#send cannot send functions as payload'

    @_log message-name, options
    request-data =
      name: message-name
      sender: @service-name
      id: uuid.v1!
    request-data.payload = payload if payload?
    request-data.response-to = options.response-to if options.response-to
    @socket.send JSON.stringify request-data
    @last-sent-id = request-data.id


  _log: (message-name, options) ->
    | options.response-to  =>  debug "sending message '#{message-name}' in response to '#{options.response-to}'"
    | _                    =>  debug "sending message '#{message-name}'"



module.exports = MessageSender
