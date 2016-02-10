require! {
  './command-manager' : HandlerManager
  './command-sender' : CommandSender
  'events' : {EventEmitter}
  './http-listener' : HttpListener
  'rails-delegate' : {delegate, delegate-event}
}
debug = require('debug')('exorelay')


class ExoRelay extends EventEmitter

  ({exocomm-port} = {}) ->
    exocomm-port or throw new Error 'exocommPort not provided'

    # manages the request handlers for incoming commands
    @command-handler = new HandlerManager!

    # sends outgoing commands to Exosphere
    @command-sender = new CommandSender {exocomm-port}

    # listens to incoming commands from Exosphere
    @http-listener = new HttpListener!
      ..on 'command', @_on-incoming-command

    delegate \close \listen \port from: @, to: @http-listener
    delegate \hasHandler \registerHandler \registerHandlers from: @, to: @command-handler
    delegate-event 'error', from: [@http-listener, @command-handler, @command-sender], to: @
    delegate-event 'online', 'offline', from: @http-listener, to: @


  send: (command, payload, reply-handler) ~>
    | reply-handler and typeof reply-handler isnt 'function'  =>  return @emit 'error', Error 'The reply handler given to ExoRelay#send must be a function'

    request-id = @command-sender.send command, payload
    if reply-handler
      @command-handler.register-reply-handler request-id, reply-handler
    request-id


  _on-incoming-command: (request-data) ~>
    | !request-data.request-id  =>  return 'missing request id'
    @command-handler.handle-request request-data,
                                    reply: @command-sender.reply-method-for request-data.request-id
                                    send: @send



module.exports = ExoRelay
