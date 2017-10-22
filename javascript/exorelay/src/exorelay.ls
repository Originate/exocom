require! {
  'events' : {EventEmitter}
  './message-handler/message-manager' : HandlerManager
  'rails-delegate' : {delegate, delegate-event}
  './websocket-connector/websocket-connector' : WebSocketConnector
  'lodash/isPlainObject'
  'lodash/isFunction'
}
debug = require('debug')('exorelay')


class ExoRelay extends EventEmitter

  (@config = {}) ->
    @config.exocom-host or throw new Error 'ExoCom host not provided to Exorelay'
    @config.role or throw new Error 'Role not provided to Exorelay'
    @config.exocom-port or= 80

    # manages the request handlers for incoming messages
    @message-handler = new HandlerManager!

    # send and receives messages from Exosphere
    @websocket-connector = new WebSocketConnector config
      ..on 'message', @_on-incoming-message
      ..on 'online', @_send-routing-config


    delegate \close \connect from: @, to: @websocket-connector
    delegate \hasHandler \registerHandler \registerHandlers from: @, to: @message-handler
    delegate-event 'error', from: [@websocket-connector, @message-handler], to: @
    delegate-event 'offline', from: @websocket-connector, to: @


  send: (message-name, payload, options, reply-handler) ~>
    if isFunction(options)
      reply-handler = options
      options = {}
    if options and !isPlainObject(options)
      return @emit 'error', Error 'The third argument to ExoRelay#send must be an object (when supplying options) or a function (when supplying a reply handler)'
    if reply-handler and !isFunction(reply-handler)
      return @emit 'error', Error 'The fourth argument to ExoRelay#send must be a function'
    message = @websocket-connector.send message-name, payload, options
    if reply-handler
      @message-handler.register-reply-handler message.activity-id, reply-handler
    message


  _on-incoming-message: (request-data) ~>
    if request-data.message-name is '__status'
      @websocket-connector.send "__status-ok"
      return 'success'

    @message-handler.handle-request request-data,
                                    reply: @websocket-connector.reply-method-for request-data.activity-id, request-data.auth, request-data.is-security
                                    send: @send


  _send-routing-config: ~>
    @send 'exocom.register-service' do
      client-name: @config.role
    @emit 'online', @websocket-connector.exocom-port


module.exports = ExoRelay
