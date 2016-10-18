require! {
  'events' : {EventEmitter}
  'ip'
  './message-handler/message-manager' : HandlerManager
  './message-sender/message-sender' : MessageSender
  'rails-delegate' : {delegate, delegate-event}
  './zmq-listener/zmq-listener' : ZmqListener
}
debug = require('debug')('exorelay')


class ExoRelay extends EventEmitter

  (@config) ->
    @config?.exocom-host or throw new Error 'ExoCom host not provided to Exorelay'
    @config?.exocom-port or throw new Error 'ExoCom port not provided to Exorelay'
    @config?.service-name or throw new Error 'Service name not provided to Exorelay'

    # manages the request handlers for incoming messages
    @message-handler = new HandlerManager!

    # sends outgoing messages to Exosphere
    @message-sender = new MessageSender config

    # listens to incoming messages from Exosphere
    @zmq-listener = new ZmqListener!
      ..on 'message', @_on-incoming-message
      ..on 'online', @_send-routing-config


    delegate \closePort, from: @, to: @message-sender
    delegate \close \listen \port from: @, to: @zmq-listener
    delegate \hasHandler \registerHandler \registerHandlers from: @, to: @message-handler
    delegate-event 'error', from: [@zmq-listener, @message-handler, @message-sender], to: @
    delegate-event 'status', 'offline', from: @zmq-listener, to: @


  send: (message-name, payload, reply-handler) ~>
    | reply-handler and typeof reply-handler isnt 'function'  =>  return @emit 'error', Error 'The reply handler given to ExoRelay#send must be a function'

    message-id = @message-sender.send message-name, payload
    if reply-handler
      @message-handler.register-reply-handler message-id, reply-handler
    message-id


  _on-incoming-message: (request-data) ~>
    if request-data.message-name is '__status'
      @message-sender.send "__status-ok"
      return 'success'

    @message-handler.handle-request request-data,
                                    reply: @message-sender.reply-method-for request-data.id
                                    send: @send


  _send-routing-config: ~>
    @send 'exorelay.register' do
      name: @config.service-name
      internal-namespace: @config.internal-namespace
      host: ip.address!
      port: @zmq-listener.port
    @emit 'online', @zmq-listener.port


module.exports = ExoRelay
