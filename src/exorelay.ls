require! {
  './message-handler/message-manager' : HandlerManager
  './message-sender/message-sender' : MessageSender
  'events' : {EventEmitter}
  './http-listener/http-listener' : HttpListener
  'rails-delegate' : {delegate, delegate-event}
}
debug = require('debug')('exorelay')


class ExoRelay extends EventEmitter

  (config) ->
    config?.exocom-port or throw new Error 'exocomPort not provided'
    config?.service-name or throw new Error 'serviceName not provided'

    # manages the request handlers for incoming messages
    @message-handler = new HandlerManager!

    # sends outgoing messages to Exosphere
    @message-sender = new MessageSender config

    # listens to incoming messages from Exosphere
    @http-listener = new HttpListener!
      ..on 'message', @_on-incoming-message

    delegate \close \listen \port from: @, to: @http-listener
    delegate \hasHandler \registerHandler \registerHandlers from: @, to: @message-handler
    delegate-event 'error', from: [@http-listener, @message-handler, @message-sender], to: @
    delegate-event 'online', 'offline', from: @http-listener, to: @


  send: (message-name, payload, reply-handler) ~>
    | reply-handler and typeof reply-handler isnt 'function'  =>  return @emit 'error', Error 'The reply handler given to ExoRelay#send must be a function'

    message-id = @message-sender.send message-name, payload
    if reply-handler
      @message-handler.register-reply-handler message-id, reply-handler
    message-id


  _on-incoming-message: (request-data) ~>
    | !request-data.id  =>  return 'missing message id'
    @message-handler.handle-request request-data,
                                    reply: @message-sender.reply-method-for request-data.id
                                    send: @send



module.exports = ExoRelay
